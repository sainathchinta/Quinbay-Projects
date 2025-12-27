package com.gdn.mta.product.config;

import java.io.ByteArrayOutputStream;

import org.springframework.core.convert.converter.Converter;
import org.springframework.core.serializer.support.SerializationFailedException;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.io.Output;
import com.esotericsoftware.kryo.pool.KryoPool;

public class KryoSerializingConverter implements Converter<Object, byte[]> {

  private final KryoPool kryoPool;

  public KryoSerializingConverter(KryoPool kryoPool) {
    this.kryoPool = kryoPool;
  }

  @Override
  public byte[] convert(Object source) {
    Kryo kryo = kryoPool.borrow();
    ByteArrayOutputStream byteStream = new ByteArrayOutputStream(256);
    Output out = new Output(byteStream);
    try {
      kryo.writeClassAndObject(out, source);
      out.flush();
      return byteStream.toByteArray();
    } catch (Throwable ex) {
      throw new SerializationFailedException("Failed to serialize object using " + this.getClass().getSimpleName(), ex);
    } finally {
      out.close();
      kryoPool.release(kryo);
    }
  }
}
