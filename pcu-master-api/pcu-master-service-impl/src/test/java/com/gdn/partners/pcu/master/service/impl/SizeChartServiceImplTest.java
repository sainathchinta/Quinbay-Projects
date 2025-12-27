package com.gdn.partners.pcu.master.service.impl;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.master.client.feign.PCBFeign;
import com.gdn.partners.pcu.master.client.feign.XProductFeign;
import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.client.model.SizeChartDeletionEventModel;
import com.gdn.partners.pcu.master.client.model.SizeChartFilterRequest;
import com.gdn.partners.pcu.master.client.model.SizeChartFilterResponse;
import com.gdn.partners.pcu.master.client.model.SizeChartRequest;
import com.gdn.partners.pcu.master.client.model.SizeChartResponse;
import com.gdn.partners.pcu.master.model.ErrorCodes;
import com.gdn.partners.pcu.master.service.impl.exception.ValidationException;
import com.gdn.x.product.model.vo.ProductSkuSizeChartResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;

import com.gdn.partners.pcu.master.properties.KafkaTopicProperties;
import com.gdn.partners.pcu.master.service.impl.config.KafkaPublisher;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.Collections;

import static com.gdn.partners.pcu.master.model.Constants.INT_ZERO;
import static com.gdn.partners.pcu.master.model.Constants.ONE;
import static com.gdn.partners.pcu.master.model.Constants.REQUEST_ID;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class SizeChartServiceImplTest {

  private static final String STORE_ID = "10001";
  private static final String SIZE_CHART_CODE = "SC-001";
  private static final String SIZE_CHART_NAME = "sizeChartName";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String DELETE_SIZE_CHART_TOPIC_NAME = "deleteSizeChartTopicName";


  @InjectMocks
  private SizeChartServiceImpl sizeChartService;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private XProductFeign xProductFeign;

  @Mock
  private KafkaPublisher kafkaPublisher;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  private SizeChartRequest sizeChartRequest;
  private SizeChartResponse sizeChartResponse;

  @BeforeEach
  void beforeEach() {
    sizeChartRequest = new SizeChartRequest();
    sizeChartResponse = new SizeChartResponse();
    sizeChartResponse.setSizeChartCode(SIZE_CHART_CODE);
  }

  @AfterEach
  void teardown() {
    Mockito.verifyNoMoreInteractions(pcbFeign);
    Mockito.verifyNoMoreInteractions(clientParameterHelper, xProductFeign);
  }

  @Test
  void upsertSizeChart() {
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);
    sizeChartRequest.setName(SIZE_CHART_NAME);
    when(pcbFeign.upsertSizeChart(any())).thenReturn(response);
    GdnBaseRestResponse serviceResponse =
        sizeChartService.upsertSizeChart(STORE_ID, sizeChartRequest);
    Mockito.verify(pcbFeign).upsertSizeChart(any());
    Mockito.verify(clientParameterHelper).isExternal();
    Assertions.assertTrue(serviceResponse.isSuccess());
  }

  @Test
  void upsertSizeChartTest() {
    GdnBaseRestResponse response =
        new GdnBaseRestResponse(ErrorCodes.SIZE_CHART_NAME_EDIT_NOT_ALLOWED_ERROR_CODE.getErrorMessage(),
            ErrorCodes.SIZE_CHART_NAME_EDIT_NOT_ALLOWED_ERROR_CODE.getErrorCode(), false, REQUEST_ID);
    sizeChartRequest.setName(SIZE_CHART_NAME);
    when(pcbFeign.upsertSizeChart(any())).thenReturn(response);
    try {
      sizeChartService.upsertSizeChart(STORE_ID, sizeChartRequest);
    } catch (ValidationException exception) {
      Assertions.assertEquals(ErrorCodes.SIZE_CHART_NAME_EDIT_NOT_ALLOWED_ERROR_CODE.getErrorMessage(),
          exception.getMessage());
    } finally {
      Mockito.verify(pcbFeign).upsertSizeChart(any());
      Mockito.verify(clientParameterHelper).isExternal();
    }
  }

  @Test
  void upsertSizeChartExceptionTest() {
    String longString =
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor "
            + "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud"
            + " exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.";
    sizeChartRequest.setName(longString);
    try {
      sizeChartService.upsertSizeChart(STORE_ID, sizeChartRequest);
    } catch (Exception e) {
      Mockito.verify(clientParameterHelper).isExternal();
    }
  }

  @Test
  void upsertNullSizeChartNameTest() {
    try {
      sizeChartService.upsertSizeChart(STORE_ID, sizeChartRequest);
    } catch (Exception e) {
      Mockito.verify(clientParameterHelper).isExternal();
    }
  }

  @Test
  void fetchSizeChart() {
    GdnRestSingleResponse<SizeChartResponse> response =
      new GdnRestSingleResponse<>(sizeChartResponse, REQUEST_ID);
    when(pcbFeign.fetchSizeChartDetails(SIZE_CHART_CODE, false)).thenReturn(response);
    SizeChartResponse serviceResponse = sizeChartService.fetchSizeChart(SIZE_CHART_CODE, false);
    Mockito.verify(pcbFeign).fetchSizeChartDetails(SIZE_CHART_CODE, false);
    Assertions.assertEquals(SIZE_CHART_CODE, serviceResponse.getSizeChartCode());
  }

  @Test
  void deleteSizeChart() {
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);
    GdnRestListResponse<ProductSkuSizeChartResponse> xProductResponse = new GdnRestListResponse();
    xProductResponse.setContent(new ArrayList<>());
    xProductResponse.setSuccess(true);
    when(xProductFeign.checkAnyProductsMappedToSizeChart(INT_ZERO, ONE, SIZE_CHART_CODE)).thenReturn(xProductResponse);
    when(pcbFeign.updateSizeChartStatus(SIZE_CHART_CODE, true, true,null)).thenReturn(response);
    when(kafkaTopicProperties.getDeleteSizeChartEventName()).thenReturn(DELETE_SIZE_CHART_TOPIC_NAME);
    sizeChartService.deleteSizeChart(SIZE_CHART_CODE);
    Mockito.verify(pcbFeign).updateSizeChartStatus(SIZE_CHART_CODE, true, true, null);
    Mockito.verify(kafkaTopicProperties).getDeleteSizeChartEventName();
    Mockito.verify(kafkaPublisher).send(DELETE_SIZE_CHART_TOPIC_NAME, SIZE_CHART_CODE,
        SizeChartDeletionEventModel.builder().sizeChartCode(SIZE_CHART_CODE).build());
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).isExternal();
  }

  @Test
  public void deleteSizeChartTest_Failure() {
    ProductSkuSizeChartResponse productSkuSizeChartResponse = new ProductSkuSizeChartResponse();
    GdnRestListResponse<ProductSkuSizeChartResponse> xProductResponse = new GdnRestListResponse();
    xProductResponse.setContent(Collections.singletonList(productSkuSizeChartResponse));
    xProductResponse.setSuccess(true);
    when(
        xProductFeign.checkAnyProductsMappedToSizeChart(INT_ZERO, ONE, SIZE_CHART_CODE)).thenReturn(
        xProductResponse);
    try {
      Assertions.assertThrows(ValidationException.class,
          () -> sizeChartService.deleteSizeChart(SIZE_CHART_CODE));
    } finally {
      Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
      Mockito.verify(clientParameterHelper).isExternal();
    }
  }

  @Test
  void filterSizeChartTest() {
    GdnRestListResponse<SizeChartFilterResponse> response =
        new GdnRestListResponse<>(null, null, true, null);
    SizeChartFilterRequest request = new SizeChartFilterRequest();
    when(pcbFeign.filter(PAGE, SIZE, request)).thenReturn(response);
    GdnRestListResponse<SizeChartFilterResponse> actualReponse =
        sizeChartService.filter(PAGE, SIZE, request);
    Mockito.verify(pcbFeign).filter(PAGE, SIZE, request);
    Assertions.assertTrue(actualReponse.isSuccess());
    Assertions.assertEquals(response.getContent(), actualReponse.getContent());
  }

  @Test
  void validateSizeChartTest() {
    GdnRestSingleResponse<SizeChartResponse> response =
        new GdnRestSingleResponse<>(sizeChartResponse, REQUEST_ID);
    when(pcbFeign.findBySizeChartNameAndBusinessPartnerCode(SIZE_CHART_NAME,
        BUSINESS_PARTNER_CODE)).thenReturn(response);
    GdnRestSingleResponse<SizeChartResponse> serviceResponse =
        sizeChartService.validate(SIZE_CHART_NAME, BUSINESS_PARTNER_CODE);
    Mockito.verify(pcbFeign)
        .findBySizeChartNameAndBusinessPartnerCode(SIZE_CHART_NAME, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(SIZE_CHART_CODE, serviceResponse.getValue().getSizeChartCode());
  }

  @Test
  void validateCategoryTest() {
    SimpleBooleanResponse simpleBooleanResponse = new SimpleBooleanResponse();
    simpleBooleanResponse.setResult(true);
    GdnRestSingleResponse<SimpleBooleanResponse> response =
        new GdnRestSingleResponse<>(simpleBooleanResponse, REQUEST_ID);
    when(pcbFeign.validateCategoryCode(CATEGORY_CODE, SIZE_CHART_CODE)).thenReturn(response);
    sizeChartService.validateCategory(CATEGORY_CODE, SIZE_CHART_CODE);
    Mockito.verify(pcbFeign).validateCategoryCode(CATEGORY_CODE, SIZE_CHART_CODE);
  }
}
