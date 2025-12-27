package com.gdn.x.productcategorybase.service.impl;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.modelmapper.ModelMapper;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;

class MapperServiceImplTest {
  private static final String DATA = "data";

  @InjectMocks
  private MapperServiceImpl mapperService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ModelMapper modelMapper;

  @BeforeEach
  public void init() {
    MockitoAnnotations.openMocks(this);
  }

  @Test
  public void mapBeanModelMapperTest() {
    ReflectionTestUtils.setField(mapperService, "useModelMapperBean", true);
    Mockito.when(modelMapper.map(DATA, String.class)).thenReturn(DATA);
    mapperService.mapBean(DATA, String.class);
    Mockito.verify(modelMapper).map(DATA, String.class);
  }

  @Test
  public void mapBeanObjectMapperTest() {
    ReflectionTestUtils.setField(mapperService, "useModelMapperBean", false);
    Mockito.when(objectMapper.convertValue(DATA, String.class)).thenReturn(DATA);
    mapperService.mapBean(DATA, String.class);
    Mockito.verify(objectMapper).convertValue(DATA, String.class);
  }

  @Test
  public void mapBeanObjectMapperSourceNullTest() {
    ReflectionTestUtils.setField(mapperService, "useModelMapperBean", false);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> mapperService.mapBean(null, String.class));
  }
}