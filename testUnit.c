// TestC.cpp : Defines the entry point for the console application.
//

/* 基本数据类型重定义 */
typedef signed char          siint08;
typedef unsigned char   unint08;
typedef signed short     siint16;
typedef unsigned short  unint16;
typedef signed int          siint32;
typedef unsigned int    unint32;
typedef float           float32;
typedef double          float64;
typedef void (*FunPtr)(void);

typedef struct TAG_ES_INPUT
{
     unint32 oneLo16;
     unint32 oneHi16;

     unint32 twoLo16;
     unint32 twoHi16;

     unint32 roll;
     unint32 pitch;
} SESInput;

typedef struct TAG_ES_DATA
{
     float32 phi;
     float32 theta;                      /* ES输入角度 */
     float32 phiLast;
     float32 thetaLast;                  /* 上周期ES角度 */
} SESData;

typedef union TAG_INPUT_IO1
{
     unint32 ui32;

     struct TAG_INPUT_IO1_BIT
     {
          unsigned BLANK    : 11;
          unsigned EPA      : 1;
          unsigned EPB      : 1;
          unsigned BAK_D13  : 1;
          unsigned BAK_D14  : 1;
          unsigned BAK_D15  : 1;

          unsigned BAK_HI16 : 16;
     } bit;
} UInpIO1;

typedef struct TAG_InSys_Flg
{
     unint32 ESInsys;

} SInSysFlg;

typedef struct TAG_ORBIT_ELEMENTS
{
     float32 a;
     float32 e;
     float32 i;
     float32 OMEGA;
     float32 w;
     float32 M0;
} SOrbitElements;

typedef struct TAG_ORBIT_CALINJ
{
     float64     T0;
     SOrbitElements orb;
} SOrbitCalInj;

typedef struct TAG_ORBIT_CAL
{
     float64     T0;   
     float32     us; 
     SOrbitElements orb;
     float32 u;
     float32 we;
} SOrbitCal;

typedef struct TAG_TIME
{
     float64 mSysTime;
     float64 mAttTime;
     float64 mLastSysTime;
}TIME_DATA;

SESInput mESInput;
SESData  mESData;
SInSysFlg   mInSysFlg;
UInpIO1          uInpIO1;

SOrbitCalInj          mOrbitCalInj;
SOrbitCal           mOrbitCal;
TIME_DATA Time;
unint32 flgUploadOrbit;

void InputProceedES(void)
{
     unint32 tmpESData;
     unint32 tmpESRollOne;
     unint32 tmpESPitchOne;
     unint32 tmpESRollTwo;
     unint32 tmpESPitchTwo;

     tmpESData = (mESInput.oneHi16 << 16) | mESInput.oneLo16;
     tmpESRollOne = (tmpESData & 0x0003FF80) >> 7;
     tmpESPitchOne= (tmpESData & 0xFFFC0000) >> 18;

     tmpESData = (mESInput.twoHi16 << 16) | mESInput.twoLo16;
     tmpESRollTwo = (tmpESData & 0x0003FF80) >> 7;
     tmpESPitchTwo= (tmpESData & 0xFFFC0000) >> 18;

     mESData.phiLast = mESData.phi;
     mESData.thetaLast = mESData.theta;
    
     if (mInSysFlg.ESInsys == 0x1)
     {
          if (uInpIO1.bit.EPA == 1)
          {
               mESInput.roll = tmpESRollOne;
               mESInput.pitch = tmpESPitchOne;

               /* BUG: 将数据转换为浮点数, 单位弧度, 忘记了乘以 度到弧度的系数 */
               mESData.phi  = (float32)((siint32)(mESInput.roll << 21) / 0x200000 * 0.1f);
               mESData.theta= (float32)((siint32)(mESInput.pitch<< 18) / 0x040000 * 0.1f);
          }
     }
     else if (mInSysFlg.ESInsys == 0x2)
     {
          if (uInpIO1.bit.EPB == 1)   /* ESB的EP可见 */
          {
               mESInput.roll = tmpESRollTwo;
               mESInput.pitch = tmpESPitchTwo;

               /* BUG: 将数据转换为浮点数, 单位弧度, 忘记了乘以 度到弧度的系数 */
               mESData.phi  = (float32)((siint32)(mESInput.roll << 21) / 0x200000 * 0.1f);
               mESData.theta= (float32)((siint32)(mESInput.pitch<< 18) / 0x040000 * 0.1f);
          }
     }
    
     return;
}

void InjOrbitProceed(void)
{
     mOrbitCal.T0 = mOrbitCalInj.T0;
     /* BUG: 单位米, 忘记了乘以 千米到米的系数 */
     mOrbitCal.orb.a = mOrbitCalInj.orb.a;
     mOrbitCal.orb.e = mOrbitCalInj.orb.e;
     mOrbitCal.orb.i = mOrbitCalInj.orb.i;
     mOrbitCal.orb.w = mOrbitCalInj.orb.w;
     mOrbitCal.orb.OMEGA = mOrbitCalInj.orb.OMEGA;
     mOrbitCal.orb.M0 = mOrbitCalInj.orb.M0;

     flgUploadOrbit = 0;

     return;
}

void SunCalculate(void)
{
     float64 tmpT;
     float64 ws, Ms, fs;

     tmpT = Time.mAttTime / 3155760000.0;

     ws = 100.0 + (0.1 * tmpT);
     Ms = 3.0 + (12.0 * tmpT);
     /* BUG: 三角函数的参数单位是弧度, 忘记了乘以度到弧度的转换系数 */
     fs = Ms + 0.03342 * sin(Ms) + 0.00034903 * sin(2*Ms);

     /* 计算太阳幅角 */
     mOrbitCal.us = (float32)(ws + fs);

     return;
}
