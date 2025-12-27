package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.Objects;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.x.product.service.util.GdnMapperHelper;

@Service
public class GdnMapperHelperImpl implements GdnMapperHelper, GdnMapper {

  private static final String NULL_WARNING = "Object must not be null";

  @Autowired
  private ModelMapper modelMapper;

  @Autowired
  private ObjectMapper objectMapper;

  @Value("${use.model.mapper.bean}")
  private boolean useModelMapperBean;

  @Override
  public <T> T mapBean(Object source, Class<T> destination) {
    checkArgument(source != null, GdnMapperHelperImpl.NULL_WARNING);
    return useModelMapperBean ? modelMapper.map(source, destination) : objectMapper.convertValue(source, destination);
  }

  @Override
  public <T> T deepCopy(Object source, Class<T> destination) {
    if (Objects.isNull(source)) {
      return null;
    }
    return mapBean(source, destination);
  }
}
