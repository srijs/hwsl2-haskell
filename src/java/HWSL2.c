#include "HWSL2.h"
#include "../sl2-inl.h"

/*
 * Class:     HWSL2
 * Method:    valid
 * Signature: (Ljava/nio/ByteBuffer;)Z
 */
JNIEXPORT jboolean JNICALL Java_HWSL2_valid
  (JNIEnv *env, jclass c, jobject a) {
  sl2_t* bufa = (*env)->GetDirectBufferAddress(env, a);
  return sl2_valid(*bufa);
}

/*
 * Class:     HWSL2
 * Method:    eq
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;)Z
 */
JNIEXPORT jboolean JNICALL Java_HWSL2_eq
  (JNIEnv *env, jclass c, jobject a, jobject b) {
  sl2_t* bufa = (*env)->GetDirectBufferAddress(env, a);
  sl2_t* bufb = (*env)->GetDirectBufferAddress(env, b);
  return sl2_eq(*bufa, *bufb);
}

/*
 * Class:     HWSL2
 * Method:    cmp
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;)I
 */
JNIEXPORT jint JNICALL Java_HWSL2_cmp
  (JNIEnv *env, jclass c, jobject a, jobject b) {
  sl2_t* bufa = (*env)->GetDirectBufferAddress(env, a);
  sl2_t* bufb = (*env)->GetDirectBufferAddress(env, b);
  return sl2_cmp(*bufa, *bufb);
}

/*
 * Class:     HWSL2
 * Method:    unit
 * Signature: (Ljava/nio/ByteBuffer;)V
 */
JNIEXPORT void JNICALL Java_HWSL2_unit
  (JNIEnv *env, jclass c, jobject a) {
  sl2_t* bufa = (*env)->GetDirectBufferAddress(env, a);
  return sl2_unit(*bufa);
}

/*
 * Class:     HWSL2
 * Method:    mulBufRight
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;)V
 */
JNIEXPORT void JNICALL Java_HWSL2_mulBufRight
  (JNIEnv *env, jclass c, jobject a, jobject b, jlong n) {
  sl2_t* bufa = (*env)->GetDirectBufferAddress(env, a);
  unsigned char* bufb = (*env)->GetDirectBufferAddress(env, b);
  return sl2_mul_buf_right(*bufa, bufb, n);
}

/*
 * Class:     HWSL2
 * Method:    mulBufLeft
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;)V
 */
JNIEXPORT void JNICALL Java_HWSL2_mulBufLeft
  (JNIEnv *env, jclass c, jobject a, jobject b, jlong n) {
  sl2_t* bufa = (*env)->GetDirectBufferAddress(env, a);
  unsigned char* bufb = (*env)->GetDirectBufferAddress(env, b);
  return sl2_mul_buf_left(*bufa, bufb, n);
}

/*
 * Class:     HWSL2
 * Method:    mul
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;)V
 */
JNIEXPORT void JNICALL Java_HWSL2_mul
  (JNIEnv *env, jclass c, jobject z, jobject a, jobject b) {
  sl2_t* bufz = (*env)->GetDirectBufferAddress(env, z);
  sl2_t* bufa = (*env)->GetDirectBufferAddress(env, a);
  sl2_t* bufb = (*env)->GetDirectBufferAddress(env, b);
  return sl2_mul(*bufz, *bufa, *bufb);
}

/*
 * Class:     HWSL2
 * Method:    serialize
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;)V
 */
JNIEXPORT void JNICALL Java_HWSL2_serialize
  (JNIEnv *env, jclass c, jobject a, jobject b) {
  sl2_t* bufa = (*env)->GetDirectBufferAddress(env, a);
  unsigned char* bufb = (*env)->GetDirectBufferAddress(env, b);
  return sl2_serialize(*bufa, bufb);
}

/*
 * Class:     HWSL2
 * Method:    unserialize
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;)V
 */
JNIEXPORT void JNICALL Java_HWSL2_unserialize
  (JNIEnv *env, jclass c, jobject a, jobject b) {
  sl2_t* bufa = (*env)->GetDirectBufferAddress(env, a);
  unsigned char* bufb = (*env)->GetDirectBufferAddress(env, b);
  return sl2_unserialize(*bufa, bufb);
}
