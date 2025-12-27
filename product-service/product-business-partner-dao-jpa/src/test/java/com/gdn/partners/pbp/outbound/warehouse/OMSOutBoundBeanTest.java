package com.gdn.partners.pbp.outbound.warehouse;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Arrays;
import java.util.List;

import com.blibli.oss.backend.common.model.response.Response;
import com.gda.mta.product.dto.UomStockValidationRequest;
import com.gda.mta.product.dto.response.UomStockValidationResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.outbound.warehouse.feign.OMSFeign;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

public class OMSOutBoundBeanTest {

  @InjectMocks
  private OMSOutBoundBean omsOutBoundBean;

  @Mock
  private OMSFeign omsFeign;

  // Test constants
  private static final String ITEM_CODE_1 = "ITEM-001";
  private static final String ITEM_CODE_2 = "ITEM-002";
  private static final String ITEM_CODE_3 = "ITEM-003";

  @BeforeEach
  public void setUp() {
    initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.reset(omsFeign);
  }

  @Test
  public void validateUomEditable_Success_NonNullCase() {
    // Given
    UomStockValidationRequest request = createUomStockValidationRequest();
    List<UomStockValidationResponse> expectedData = createUomStockValidationResponseList();

    Response<List<UomStockValidationResponse>> response = new Response<>();
    response.setData(expectedData);

    Mockito.when(omsFeign.validateUomEditable(eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            eq(GdnMandatoryRequestParameterUtil.getClientId()),
            eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            eq(GdnMandatoryRequestParameterUtil.getUsername()),
            any(UomStockValidationRequest.class)))
        .thenReturn(response);

    // When
    List<UomStockValidationResponse> result = omsOutBoundBean.validateUomEditable(request);

    // Then
    Assertions.assertNotNull(result);
    Assertions.assertEquals(expectedData.size(), result.size());
    Assertions.assertEquals(expectedData, result);

    Mockito.verify(omsFeign).validateUomEditable(eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        eq(GdnMandatoryRequestParameterUtil.getClientId()),
        eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        eq(GdnMandatoryRequestParameterUtil.getUsername()), any(UomStockValidationRequest.class));
  }

  @Test
  public void validateUomEditable_Failure_ResponseNull() {
    // Given
    UomStockValidationRequest request = createUomStockValidationRequest();

    Mockito.when(omsFeign.validateUomEditable(eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            eq(GdnMandatoryRequestParameterUtil.getClientId()),
            eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            eq(GdnMandatoryRequestParameterUtil.getUsername()),
            any(UomStockValidationRequest.class)))
        .thenReturn(null);

    // When & Then
    ApplicationRuntimeException exception =
        Assertions.assertThrows(ApplicationRuntimeException.class,
            () -> omsOutBoundBean.validateUomEditable(request));

    Assertions.assertEquals(ErrorCategory.UNSPECIFIED, exception.getErrorCodes());
    Assertions.assertEquals("Unspecified error :Client Exception when calling Feign",
        exception.getErrorMessage());

    Mockito.verify(omsFeign).validateUomEditable(eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        eq(GdnMandatoryRequestParameterUtil.getClientId()),
        eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        eq(GdnMandatoryRequestParameterUtil.getUsername()), any(UomStockValidationRequest.class));
  }

  @Test
  public void validateUomEditable_Failure_ResponseDataNull() {
    // Given
    UomStockValidationRequest request = createUomStockValidationRequest();

    Response<List<UomStockValidationResponse>> response = new Response<>();
    response.setData(null); // Data is null

    Mockito.when(omsFeign.validateUomEditable(eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            eq(GdnMandatoryRequestParameterUtil.getClientId()),
            eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            eq(GdnMandatoryRequestParameterUtil.getUsername()),
            any(UomStockValidationRequest.class)))
        .thenReturn(response);

    // When & Then
    ApplicationRuntimeException exception =
        Assertions.assertThrows(ApplicationRuntimeException.class,
            () -> omsOutBoundBean.validateUomEditable(request));

    Assertions.assertEquals(ErrorCategory.UNSPECIFIED, exception.getErrorCodes());
    Assertions.assertEquals("Unspecified error :Client Exception when calling Feign",
        exception.getErrorMessage());

    Mockito.verify(omsFeign).validateUomEditable(eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        eq(GdnMandatoryRequestParameterUtil.getClientId()),
        eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        eq(GdnMandatoryRequestParameterUtil.getUsername()), any(UomStockValidationRequest.class));
  }

  private UomStockValidationRequest createUomStockValidationRequest() {
    UomStockValidationRequest request = new UomStockValidationRequest();
    request.setItemCodes(Arrays.asList(ITEM_CODE_1, ITEM_CODE_2, ITEM_CODE_3));
    return request;
  }

  private List<UomStockValidationResponse> createUomStockValidationResponseList() {
    UomStockValidationResponse response1 = new UomStockValidationResponse();
    response1.setItemCode(ITEM_CODE_1);
    response1.setHasExistingStock(true);
    response1.setHasPendingDocuments(false);
    response1.setSuccess(true);
    response1.setErrorMessage(null);

    UomStockValidationResponse response2 = new UomStockValidationResponse();
    response2.setItemCode(ITEM_CODE_2);
    response2.setHasExistingStock(false);
    response2.setHasPendingDocuments(true);
    response2.setSuccess(false);
    response2.setErrorMessage("Item has pending documents");

    return Arrays.asList(response1, response2);
  }
}
