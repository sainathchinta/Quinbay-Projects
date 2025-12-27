package com.gdn.x.productcategorybase.service.impl;


import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.dto.request.DimensionMappingRequest;
import com.gdn.x.productcategorybase.dto.request.DimensionMappingUpdateRequest;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.exception.ValidationException;
import com.gdn.x.productcategorybase.service.DimensionMappingService;
import com.gdn.x.productcategorybase.service.MasterAttributeService;

public class DimensionMappingWrapperServiceBeanTest {
  private static final String STORE_ID = "STORE_ID";
  private static final String DIMENSION_ID_1 = "DIMENSION_ID_1";
  private static final String DIMENSION_ID_2 = "DIMENSION_ID_2";
  private static final String DIMENSION_ID_3 = "DIMENSION_ID_3";

  private static final String ATTRIBUTE_CODE = "AC-12345";

  private Attribute attribute;
  private DimensionMappingUpdateRequest dimensionMappingUpdateRequest;

  @InjectMocks
  private DimensionMappingWrapperServiceBean dimensionMappingWrapperServiceBean;

  @Mock
  private MasterAttributeService masterAttributeService;

  @Mock
  private DimensionMappingService dimensionMappingService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    attribute.setSizeAttribute(true);
    dimensionMappingUpdateRequest = new DimensionMappingUpdateRequest();
    DimensionMappingRequest dimensionMappingRequest1 = new DimensionMappingRequest();
    dimensionMappingRequest1.setDimensionId(DIMENSION_ID_1);
    dimensionMappingUpdateRequest.setAddedDimensionMapping(List.of(dimensionMappingRequest1));
    DimensionMappingRequest dimensionMappingRequest2 = new DimensionMappingRequest();
    dimensionMappingRequest2.setDimensionId(DIMENSION_ID_2);
    dimensionMappingUpdateRequest.setUpdateDimensionMapping(
      new ArrayList<>(List.of(dimensionMappingRequest2)));
    DimensionMappingRequest dimensionMappingRequest3 = new DimensionMappingRequest();
    dimensionMappingRequest3.setDimensionId(DIMENSION_ID_3);
    dimensionMappingUpdateRequest.setDeletedDimensionMapping(List.of(dimensionMappingRequest3));
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(dimensionMappingService);
    Mockito.verifyNoMoreInteractions(masterAttributeService);
  }

  @Test
  public void updateDimensionMappingTest() {
    Mockito.when(masterAttributeService.findDetailByAttributeCode(ATTRIBUTE_CODE))
      .thenReturn(attribute);
    dimensionMappingWrapperServiceBean.updateDimensionMapping(STORE_ID, ATTRIBUTE_CODE,
      dimensionMappingUpdateRequest);
    Mockito.verify(masterAttributeService).findDetailByAttributeCode(ATTRIBUTE_CODE);
    Mockito.verify(dimensionMappingService)
      .updateDimensionMapping(STORE_ID, attribute, dimensionMappingUpdateRequest);
  }

  @Test
  public void updateDimensionMappingInvalidRequestTest() {
    Mockito.when(masterAttributeService.findDetailByAttributeCode(Mockito.any()))
      .thenReturn(attribute);
    DimensionMappingRequest dimensionMappingRequest = new DimensionMappingRequest();
    dimensionMappingRequest.setDimensionId(DIMENSION_ID_1);
    dimensionMappingUpdateRequest.getUpdateDimensionMapping().add(dimensionMappingRequest);
    Assertions.assertThrows(ValidationException.class, () -> dimensionMappingWrapperServiceBean.updateDimensionMapping(STORE_ID, ATTRIBUTE_CODE,
      dimensionMappingUpdateRequest));
  }
}
