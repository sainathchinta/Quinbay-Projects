package com.gdn.mta.product.config;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.convert.converter.Converter;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.SerializationException;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.pool.KryoFactory;
import com.esotericsoftware.kryo.pool.KryoPool;


public class KryoSerializer implements RedisSerializer<Object> {

  private static final Logger LOG = LoggerFactory.getLogger(KryoSerializer.class);
  private final KryoFactory factory = new KryoFactory() {
    @Override
    public Kryo create() {
      Kryo kryo = new Kryo();
      kryo.setRegistrationRequired(false);
      return kryo;
    }
  };

  private final KryoPool pool = new KryoPool.Builder(factory).softReferences().build();
  private final Converter<Object, byte[]> serializer = new KryoSerializingConverter(pool);
  private final Converter<byte[], Object> deserializer = new KryoDeserializingConverter(pool);

  @Override
  public Object deserialize(byte[] bytes) {
    if (SerializationUtils.isEmpty(bytes)) {
      return null;
    }
    try {
      return deserializer.convert(bytes);
    } catch (Exception e) {
      LOG.error(e.getMessage(), e);
      throw new SerializationException("Cannot deserialize", e);
    }
  }

  @Override
  public byte[] serialize(Object object) {
    if (object == null) {
      return SerializationUtils.EMPTY_ARRAY;
    }
    try {
      return serializer.convert(object);
    } catch (Exception e) {
      LOG.error(e.getMessage(), e);
      throw new SerializationException("Cannot serialize", e);
    }
  }

}
