package hwsl2;

import java.nio.ByteBuffer;
import java.nio.charset.*;

public class HWSL2 {

  static {
    System.loadLibrary("hwsl2");
  }
  private static native boolean valid(ByteBuffer a);
  private static native boolean eq(ByteBuffer a, ByteBuffer b);
  private static native int cmp(ByteBuffer a, ByteBuffer b);
  private static native void copy(ByteBuffer a, ByteBuffer b);
  private static native void unit(ByteBuffer a);
  private static native void mulBufRight(ByteBuffer a, byte[] buf, long n);
  private static native void mulBufLeft(ByteBuffer a, byte[] buf, long n);
  private static native void mul(ByteBuffer z, ByteBuffer a, ByteBuffer b);
  private static native void serialize(ByteBuffer a, byte[] buf);
  private static native void unserialize(ByteBuffer a, byte[] buf);

  private ByteBuffer buf;

  public HWSL2() {
    buf = ByteBuffer.allocateDirect(64);
  }

  public void reset() {
    unit(buf);
  }

  public void copy(HWSL2 b) {
    copy(b.buf, buf);
  }

  public boolean valid() {
    return valid(buf);
  }

  public boolean eq(HWSL2 b) {
    return eq(buf, b.buf);
  }

  public int cmp(HWSL2 b) {
    return cmp(buf, b.buf);
  }

  public void append(byte[] b, long n) {
    mulBufRight(buf, b, n);
  }

  public void prepend(byte[] b, long n) {
    mulBufLeft(buf, b, n);
  }

  public HWSL2 mul(HWSL2 b) {
    HWSL2 z = new HWSL2();
    mul(z.buf, buf, b.buf);
    return z;
  }

  public String serialize() {
    byte[] bytes = new byte[86];
    serialize(buf, bytes);
    return new String(bytes, StandardCharsets.US_ASCII);
  }

  public void unserialize(String str) {
    byte[] bytes = str.getBytes(StandardCharsets.US_ASCII);
    unserialize(buf, bytes);
  }

}
