package com.gdn.x.productcategorybase.service.impl;

import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.DimensionFilterRequest;
import com.gdn.x.productcategorybase.dto.request.DimensionRequest;
import com.gdn.x.productcategorybase.dto.response.DimensionResponse;
import com.gdn.x.productcategorybase.entity.Dimension;
import com.gdn.x.productcategorybase.exception.ValidationException;

import com.gdn.x.productcategorybase.repository.DimensionRepository;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import java.util.Collections;

public class DimensionServiceBeanTest {

  private static final String STORE_ID = "STORE_ID";
  private static final String DIMENSION_CODE = "DIM-000001";
  private static final String DIMENSION_NAME = "DIMENSION_NAME";
  private static final String DIMENSION_NAME_ENGLISH = "DIMENSION_NAME_ENGLISH";
  private static final byte[] DESCRIPTION = new byte[2];
  private static final String SORT_FIELD = "name";
  private static final String SORT_DIRECTION = "asc";

  @InjectMocks
  private DimensionServiceBean dimensionServiceBean;

  @Mock
  private DimensionRepository dimensionRepository;

  private Dimension dimension;

  private DimensionRequest dimensionRequest;

  private DimensionFilterRequest dimensionFilterRequest;
  private Page<Dimension> dimensionPage;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    dimension = new Dimension();
    dimension.setDimensionCode(DIMENSION_CODE);
    dimension.setStoreId(STORE_ID);
    dimension.setDimensionType(AttributeType.DEFINING_ATTRIBUTE);
    dimension.setName(DIMENSION_NAME);
    dimensionRequest = DimensionRequest.builder().name(DIMENSION_NAME).build();
    dimensionFilterRequest = DimensionFilterRequest.builder().keyword(DIMENSION_NAME).build();
    dimensionPage = new PageImpl<>(Collections.singletonList(dimension));
  }

  public void tearDown() {
    Mockito.verifyNoMoreInteractions(dimensionRepository);
  }

  @Test
  public void getDimensionDetailTest() {
    Mockito.when(dimensionRepository.findByStoreIdAndMarkForDeleteAndDimensionCode(STORE_ID, false,
      DIMENSION_CODE)).thenReturn(dimension);
    DimensionResponse response =
      dimensionServiceBean.fetchDimensionDetails(STORE_ID, DIMENSION_CODE);
    Mockito.verify(dimensionRepository)
      .findByStoreIdAndMarkForDeleteAndDimensionCode(STORE_ID, false, DIMENSION_CODE);
    Assertions.assertEquals(DIMENSION_CODE, response.getDimensionCode());
  }

  @Test
  public void saveNewDimension_success() {
    Mockito.when(dimensionRepository.findByStoreIdAndNameIgnoreCase(STORE_ID, DIMENSION_NAME))
        .thenReturn(null);
    Mockito.when(dimensionRepository.getSequenceByAttributeCode("DIM")).thenReturn(1l);
    dimensionServiceBean.save(STORE_ID,dimensionRequest);
    Mockito.verify(dimensionRepository).save(dimension);
  }

  @Test
  public void saveNewDimension_successWithDescription(){
    dimensionRequest.setDescription(new byte[]{1,2});
    Mockito.when(dimensionRepository.findByStoreIdAndNameIgnoreCase(STORE_ID, DIMENSION_NAME))
        .thenReturn(null);
    Mockito.when(dimensionRepository.getSequenceByAttributeCode("DIM")).thenReturn(1l);
    dimensionServiceBean.save(STORE_ID,dimensionRequest);
    Mockito.verify(dimensionRepository).save(dimension);
  }

  @Test
  public void saveNewDimension_duplicateDimension() {
    Mockito.when(dimensionRepository.findByStoreIdAndNameIgnoreCase(STORE_ID, DIMENSION_NAME))
        .thenReturn(new Dimension());
    try {
      dimensionServiceBean.save(STORE_ID, dimensionRequest);
    } catch (ValidationException e) {
      Assertions.assertEquals(ErrorMessage.DIMENSION_ALREADY_EXISTS_ERROR_MESSAGE.getMessage(),
          e.getErrorMessage());
    }
  }

  @Test
  public void filterDimension_success() {
    Mockito.when(
            dimensionRepository.findByStoreIdAndKeywordAndMarkForDeleteFalseOrderByName(STORE_ID,
                DIMENSION_NAME, SORT_FIELD, SORT_DIRECTION, PageRequest.of(0, 10)))
        .thenReturn(dimensionPage);
    Page<DimensionResponse> dimensionResponses =
        dimensionServiceBean.filter(STORE_ID, dimensionFilterRequest, PageRequest.of(0, 10));
    Mockito.verify(
        dimensionRepository).findByStoreIdAndKeywordAndMarkForDeleteFalseOrderByName(STORE_ID,
            DIMENSION_NAME, SORT_FIELD, SORT_DIRECTION, PageRequest.of(0, 10));
    Assertions.assertEquals(1, dimensionResponses.getTotalElements());
  }

  @Test
  public void editDimension_success() {
    dimensionRequest.setDimensionCode(DIMENSION_CODE);
    dimensionRequest.setName(DIMENSION_NAME_ENGLISH);
    dimensionRequest.setDescription(DESCRIPTION);
    Mockito.when(dimensionRepository.findByStoreIdAndMarkForDeleteAndDimensionCode(STORE_ID, false ,DIMENSION_CODE))
        .thenReturn(dimension);
    dimensionServiceBean.edit(STORE_ID,dimensionRequest);
    dimension.setNameEnglish(DIMENSION_NAME_ENGLISH);
    dimension.setDescription(DESCRIPTION);
    dimension.setDescriptionSearch(new String(DESCRIPTION));
    Mockito.verify(dimensionRepository).saveAndFlush(dimension);
  }

  @Test
  public void editDimension_dimensionCodeDoesNotExist() {
    Mockito.when(dimensionRepository.findByStoreIdAndMarkForDeleteAndDimensionCode(STORE_ID, false ,DIMENSION_CODE))
        .thenReturn(null);
    try {
      dimensionServiceBean.edit(STORE_ID, dimensionRequest);
    }
    catch (ValidationException e){
      Assertions.assertEquals(ErrorMessage.DIMENSION_DOES_NOT_EXIST_WITH_DIMENSION_CODE.getMessage(),
          e.getErrorMessage());
    }
  }

  @Test
  public void findByIdTest() {
    Mockito.when(
            dimensionRepository.findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, DIMENSION_CODE))
        .thenReturn(null);
    dimensionServiceBean.findById(STORE_ID, DIMENSION_CODE);
    Mockito.verify(dimensionRepository)
        .findByStoreIdAndIdAndMarkForDeleteFalse(STORE_ID, DIMENSION_CODE);
  }

  @Test
  public void validateTest_noDuplicate() {
    Mockito.when(
            dimensionRepository.findByStoreIdAndNameIgnoreCase(STORE_ID, DIMENSION_NAME))
        .thenReturn(dimension);
    Assertions.assertNotNull(dimensionServiceBean.findByName(STORE_ID, DIMENSION_NAME));
    Mockito.verify(dimensionRepository)
        .findByStoreIdAndNameIgnoreCase(STORE_ID, DIMENSION_NAME);
  }

  @Test
  public void validateTest_duplicatePresent() {
    Mockito.when(
            dimensionRepository.findByStoreIdAndNameIgnoreCase(STORE_ID, DIMENSION_NAME))
        .thenReturn(null);
    Assertions.assertNull(dimensionServiceBean.findByName(STORE_ID, DIMENSION_NAME));
    Mockito.verify(dimensionRepository)
        .findByStoreIdAndNameIgnoreCase(STORE_ID, DIMENSION_NAME);
  }
}
