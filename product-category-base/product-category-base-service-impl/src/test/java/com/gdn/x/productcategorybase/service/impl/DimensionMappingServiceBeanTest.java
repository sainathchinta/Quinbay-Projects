package com.gdn.x.productcategorybase.service.impl;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.DimensionMappingRequest;
import com.gdn.x.productcategorybase.dto.request.DimensionMappingUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.DimensionMappingResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Dimension;
import com.gdn.x.productcategorybase.entity.DimensionMapping;
import com.gdn.x.productcategorybase.exception.ValidationException;
import com.gdn.x.productcategorybase.repository.DimensionMappingRepository;
import com.gdn.x.productcategorybase.service.DimensionService;

public class DimensionMappingServiceBeanTest {

  private static final String STORE_ID = "STORE_ID";
  private static final String DIMENSION_CODE_1 = "DIMENSION_CODE_1";
  private static final String DIMENSION_CODE_2 = "DIMENSION_CODE_2";
  private static final String DIMENSION_CODE_3 = "DIMENSION_CODE_3";

  private static final String ATTRIBUTE_CODE = "AC-12345";
  private static final int PAGE = 0;
  private static final int SIZE = 50;
  private Page<DimensionMapping> dimensionMappingPage;
  private Dimension dimension;
  private DimensionMapping dimensionMapping;
  private DimensionMappingUpdateRequest dimensionMappingUpdateRequest;
  private Attribute attribute;

  @InjectMocks
  private DimensionMappingServiceBean dimensionMappingServiceBean;

  @Mock
  private DimensionMappingRepository dimensionMappingRepository;

  @Mock
  private DimensionService dimensionService;

  @Captor
  private ArgumentCaptor<List<DimensionMapping>> dimensionMappingArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    attribute = new Attribute();
    attribute.setAttributeCode(ATTRIBUTE_CODE);
    dimension = new Dimension();
    dimension.setDimensionCode(DIMENSION_CODE_1);
    dimensionMapping = new DimensionMapping();
    dimensionMapping.setDimension(dimension);
    dimensionMapping.setMandatory(true);
    dimensionMapping.setAttributeCode(ATTRIBUTE_CODE);
    dimensionMappingPage = new PageImpl<>(Collections.singletonList(dimensionMapping));
    dimensionMappingUpdateRequest = new DimensionMappingUpdateRequest();
    DimensionMappingRequest dimensionMappingRequest1 = new DimensionMappingRequest();
    dimensionMappingRequest1.setDimensionId(DIMENSION_CODE_2);
    dimensionMappingUpdateRequest.setAddedDimensionMapping(List.of(dimensionMappingRequest1));
    DimensionMappingRequest dimensionMappingRequest2 = new DimensionMappingRequest();
    dimensionMappingRequest2.setDimensionId(DIMENSION_CODE_1);
    dimensionMappingUpdateRequest.setUpdateDimensionMapping(List.of(dimensionMappingRequest2));
    DimensionMappingRequest dimensionMappingRequest3 = new DimensionMappingRequest();
    dimensionMappingRequest3.setDimensionId(DIMENSION_CODE_3);
    dimensionMappingUpdateRequest.setDeletedDimensionMapping(List.of(dimensionMappingRequest3));
  }

  public void tearDown() {
    Mockito.verifyNoMoreInteractions(dimensionMappingRepository);
    Mockito.verifyNoMoreInteractions(dimensionService);
  }

  @Test
  public void testFetchDimensionMapping() {
    Pageable pageable = PageRequest.of(PAGE, SIZE);
    Mockito.when(
        dimensionMappingRepository.findByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID,
            ATTRIBUTE_CODE, pageable)).thenReturn(dimensionMappingPage);
    Page<DimensionMappingResponse> response =
        dimensionMappingServiceBean.fetchDimensionMapping(ATTRIBUTE_CODE, STORE_ID, pageable);
    Mockito.verify(dimensionMappingRepository)
        .findByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE, pageable);
  }

  @Test
  public void saveNewDimension_duplicateDimension() {
    Mockito.when(
        dimensionMappingRepository.findByStoreIdAndAttributeCodeAndDimensionAndMarkForDeleteFalse(
            STORE_ID, ATTRIBUTE_CODE, dimension)).thenReturn(new DimensionMapping());
    try {
      dimensionMappingServiceBean.save(dimensionMapping);
    } catch (ValidationException e) {
      Assertions.assertEquals(
          ErrorMessage.DIMENSION_MAPPING_ALREADY_EXIST_WITH_ATTRIBUTE_CODE.getMessage(),
          e.getErrorMessage());
    }
  }

  @Test
  public void saveNewDimension_success() {
    Mockito.when(
        dimensionMappingRepository.findByStoreIdAndAttributeCodeAndDimensionAndMarkForDeleteFalse(
            STORE_ID, ATTRIBUTE_CODE, dimension)).thenReturn(null);
    dimensionMappingServiceBean.save(dimensionMapping);
    Mockito.verify(dimensionMappingRepository).save(dimensionMapping);
  }

  @Test
  public void updateDimensionMappingTest() {
    Dimension dimension1 = new Dimension();
    dimension1.setDimensionCode(DIMENSION_CODE_1);
    dimension1.setId(DIMENSION_CODE_1);
    Mockito.when(dimensionService.findById(STORE_ID, DIMENSION_CODE_1)).thenReturn(dimension1);
    Dimension dimension2 = new Dimension();
    dimension2.setDimensionCode(DIMENSION_CODE_2);
    dimension2.setId(DIMENSION_CODE_2);
    Mockito.when(dimensionService.findById(STORE_ID, DIMENSION_CODE_2)).thenReturn(dimension2);
    Dimension dimension3 = new Dimension();
    dimension3.setDimensionCode(DIMENSION_CODE_3);
    dimension3.setId(DIMENSION_CODE_3);
    Mockito.when(dimensionService.findById(STORE_ID, DIMENSION_CODE_3)).thenReturn(dimension3);
    DimensionMapping dimensionMapping1 = new DimensionMapping();
    dimensionMapping1.setDimension(dimension1);
    DimensionMapping dimensionMapping3 = new DimensionMapping();
    dimensionMapping3.setDimension(dimension3);
    Mockito.when(
      dimensionMappingRepository.findByStoreIdAndAttributeCodeAndDimensionAndMarkForDeleteFalse(
        STORE_ID, ATTRIBUTE_CODE, dimension2)).thenReturn(null);
    Mockito.when(
      dimensionMappingRepository.findByStoreIdAndAttributeCodeAndDimensionAndMarkForDeleteFalse(
        STORE_ID, ATTRIBUTE_CODE, dimension1)).thenReturn(dimensionMapping1);
    Mockito.when(
      dimensionMappingRepository.findByStoreIdAndAttributeCodeAndDimensionAndMarkForDeleteFalse(
        STORE_ID, ATTRIBUTE_CODE, dimension3)).thenReturn(dimensionMapping3);
    dimensionMappingServiceBean.updateDimensionMapping(STORE_ID, attribute,
      dimensionMappingUpdateRequest);
    Mockito.verify(dimensionService).findById(STORE_ID, DIMENSION_CODE_1);
    Mockito.verify(dimensionService).findById(STORE_ID, DIMENSION_CODE_2);
    Mockito.verify(dimensionService).findById(STORE_ID, DIMENSION_CODE_3);
    Mockito.verify(dimensionMappingRepository)
      .findByStoreIdAndAttributeCodeAndDimensionAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE,
        dimension2);
    Mockito.verify(dimensionMappingRepository)
      .findByStoreIdAndAttributeCodeAndDimensionAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE,
        dimension1);
    Mockito.verify(dimensionMappingRepository)
      .findByStoreIdAndAttributeCodeAndDimensionAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE,
        dimension3);
    Mockito.verify(dimensionMappingRepository).saveAll(dimensionMappingArgumentCaptor.capture());
    Assertions.assertEquals(3, dimensionMappingArgumentCaptor.getValue().size());
  }

  @Test
  public void updateDimensionMappingEmptyListTest() {
    dimensionMappingServiceBean.updateDimensionMapping(STORE_ID, attribute,
      new DimensionMappingUpdateRequest());
  }

  @Test
  public void testFetchDimensionMappingListTest() {
    Mockito.when(
      dimensionMappingRepository.findByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID,
        ATTRIBUTE_CODE)).thenReturn(List.of(dimensionMapping));
    List<DimensionMapping> response =
      dimensionMappingServiceBean.fetchDimensionMappingForAttribute(STORE_ID,ATTRIBUTE_CODE);
    Mockito.verify(dimensionMappingRepository)
      .findByStoreIdAndAttributeCodeAndMarkForDeleteFalse(STORE_ID, ATTRIBUTE_CODE);
  }

}
