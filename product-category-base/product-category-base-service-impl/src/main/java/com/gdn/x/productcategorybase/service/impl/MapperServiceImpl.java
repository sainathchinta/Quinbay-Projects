package com.gdn.x.productcategorybase.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.service.MapperService;

@Service
public class MapperServiceImpl implements MapperService {
  private static final String NULL_WARNING = "Object must not be null";

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ModelMapper modelMapper;

  @Value("${use.model.mapper.bean}")
  private boolean useModelMapperBean;

  @Override
  public <T> T mapBean(Object source, Class<T> destination) {
    checkArgument(source != null, NULL_WARNING);
    return useModelMapperBean ? modelMapper.map(source, destination) : objectMapper.convertValue(source, destination);
  }
}
