package com.gdn.mta.bulk.repository.pcb;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.feignConfig.PCBFeign;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;

public class ProductAttributeRepositoryBeanTest {

  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String ATTRIBUTE_CODE = "attributeCode";
  private static final String ERROR_CODE = "errorCode";
  private static final String ERROR_MESSAGE = "errorMessage";
  private static final List<String> ATTRIBUTE_CODES = Arrays.asList(ATTRIBUTE_CODE);

  @InjectMocks
  private ProductAttributeRepositoryBean productAttributeRepositoryBean;

  @Mock
  private PCBFeign pcbFeign;

  @BeforeEach
  public void setUp() {
    initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(pcbFeign);
  }

  @Test
  public void testGetAttributeDetailByAttributeCodesSuccessResponse() throws Exception {
    List<AttributeResponse> attributeResponseList = Arrays.asList(new AttributeResponse());
    GdnRestListResponse<AttributeResponse> response =
        new GdnRestListResponse<>(attributeResponseList, null, REQUEST_ID);
    when(pcbFeign.getAttributeDetailByAttributeCode(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyBoolean(),
        any(AttributeCodesRequest.class))).thenReturn(response);
        productAttributeRepositoryBean.getAttributeDetailByAttributeCodes(REQUEST_ID, USERNAME, ATTRIBUTE_CODES);
    verify(pcbFeign).getAttributeDetailByAttributeCode(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyBoolean(),
        any(AttributeCodesRequest.class));
  }

  @Test
  public void testGetAttributeDetailByAttributeCodesErrorResponse() throws Exception {
    List<AttributeResponse> attributeResponseList = Arrays.asList(new AttributeResponse());
    GdnRestListResponse<AttributeResponse> response =
        new GdnRestListResponse<>(ERROR_CODE, ERROR_MESSAGE, false, attributeResponseList, null, REQUEST_ID);
    when(pcbFeign.getAttributeDetailByAttributeCode(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyBoolean(),
        any(AttributeCodesRequest.class))).thenReturn(response);
    try {
      productAttributeRepositoryBean.getAttributeDetailByAttributeCodes(REQUEST_ID, USERNAME, ATTRIBUTE_CODES);
    } catch (ApplicationException e) {
      verify(pcbFeign).getAttributeDetailByAttributeCode(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
          Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
          Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
          Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
          Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyBoolean(),
          any(AttributeCodesRequest.class));
    }
  }

  @Test
  public void testGetAttributeDetailByAttributeCodesSuccessResponse3() throws Exception {
    List<AttributeResponse> attributeResponseList = Arrays.asList(new AttributeResponse());
    GdnRestListResponse<AttributeResponse> response =
        new GdnRestListResponse<>(attributeResponseList, null, REQUEST_ID);
    when(pcbFeign.getAttributeDetailByAttributeCode(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyBoolean(),
        any(AttributeCodesRequest.class))).thenReturn(response);
    productAttributeRepositoryBean.getAttributeDetailByAttributeCodes(REQUEST_ID, USERNAME, ATTRIBUTE_CODES, true);
    verify(pcbFeign).getAttributeDetailByAttributeCode(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyBoolean(),
        any(AttributeCodesRequest.class));
  }

  @Test
  public void testGetAttributeDetailByAttributeCodesErrorResponse4() throws Exception {
    List<AttributeResponse> attributeResponseList = Arrays.asList(new AttributeResponse());
    GdnRestListResponse<AttributeResponse> response =
        new GdnRestListResponse<>(ERROR_CODE, ERROR_MESSAGE, false, attributeResponseList, null, REQUEST_ID);
    when(pcbFeign.getAttributeDetailByAttributeCode(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyBoolean(),
        any(AttributeCodesRequest.class))).thenReturn(response);
    try {
      productAttributeRepositoryBean.getAttributeDetailByAttributeCodes(REQUEST_ID, USERNAME, ATTRIBUTE_CODES, true);
    } catch (ApplicationException e) {
      verify(pcbFeign).getAttributeDetailByAttributeCode(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
          Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
          Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
          Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
          Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyBoolean(),
          any(AttributeCodesRequest.class));
    }
  }

}
