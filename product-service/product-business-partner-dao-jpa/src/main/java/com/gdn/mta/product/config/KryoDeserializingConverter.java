package com.gdn.mta.product.config;

import java.io.ByteArrayInputStream;

import org.springframework.core.convert.converter.Converter;
import org.springframework.core.serializer.support.SerializationFailedException;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.pool.KryoPool;

public class KryoDeserializingConverter implements Converter<byte[], Object> {

  private final KryoPool kryoPool;

  public KryoDeserializingConverter(KryoPool kryoPool) {
    this.kryoPool = kryoPool;
  }

  @Override
  public Object convert(byte[] source) {
    ByteArrayInputStream byteStream = new ByteArrayInputStream(source);
    Input input = new Input(byteStream);
    Kryo kryo = kryoPool.borrow();
    try {
      return kryo.readClassAndObject(input);
    } catch (Throwable ex) {
      throw new SerializationFailedException("Failed to deserialize message " + new String(source)
          + "Is the byte array a result of corresponding serialization for "
          + this.getClass().getSimpleName() + "?", ex);
    } finally {
      input.close();
      kryoPool.release(kryo);
    }
  }

}
