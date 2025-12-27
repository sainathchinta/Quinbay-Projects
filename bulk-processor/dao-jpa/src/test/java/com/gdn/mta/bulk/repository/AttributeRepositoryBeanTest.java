package com.gdn.mta.bulk.repository;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.feignConfig.PCBFeign;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.slf4j.MDC;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.UUID;

public class AttributeRepositoryBeanTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String DEFAULT_ATTRIBUTE_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String BRAND_VALUE = "MAC";
  private static final VerificationMode AT_LEAST_ONE = Mockito.times(1);
  private static final PageMetaData PAGE_META_DATA = new PageMetaData(0L, 1L, 1L);
  private static final GdnRestListRequest LIST_REQUEST = new GdnRestListRequest(0, Integer.MAX_VALUE);
  private static final int SEQUENCE = 0;
  private static final String ATTRIBUTE_VALUE = "attributeValue";
  private static final String ATTRIBUTE_CODE = "attributeCode";

  @InjectMocks
  AttributeRepositoryBean attributeRepositoryBean;

  @Mock
  private PCBFeign pcbFeign;

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.pcbFeign);
  }

  private AttributeResponse getAttribute() throws Exception {
    Date date = Calendar.getInstance().getTime();
    AttributeResponse attribute = new AttributeResponse();
    attribute.setStoreId(DEFAULT_STORE_ID);
    attribute.setCreatedDate(date);
    attribute.setCreatedBy(DEFAULT_USERNAME);
    attribute.setUpdatedDate(date);
    attribute.setUpdatedBy(DEFAULT_USERNAME);
    return attribute;
  }

  private AttributeValueResponse getAttributeValueResponse() throws Exception {
    AttributeValueResponse response = new AttributeValueResponse();
    response.setSequence(SEQUENCE);
    response.setValue(ATTRIBUTE_VALUE);
    return response;
  }


  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void testFindOne() throws Exception {
    AttributeResponse attribute = getAttribute();
    GdnRestSingleResponse<AttributeResponse> response =
        new GdnRestSingleResponse<AttributeResponse>(null, null, true, attribute, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
    Mockito.when(pcbFeign.getAttributeDetailByAttributeId(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyString(), Mockito.anyBoolean()))
        .thenReturn(response);
    attributeRepositoryBean.findOne(DEFAULT_STORE_ID, DEFAULT_ATTRIBUTE_ID);
    Mockito.verify(pcbFeign).getAttributeDetailByAttributeId(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void testFindOneWithException() throws Exception {
    GdnRestSingleResponse<AttributeResponse> response =
        new GdnRestSingleResponse<AttributeResponse>("Read timeout", ErrorCategory.UNSPECIFIED.getCode(), false, null,
            DEFAULT_REQUEST_ID);
    Mockito.when(pcbFeign.getAttributeDetailByAttributeId(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyString(), Mockito.anyBoolean()))
        .thenReturn(response);
    try {
      attributeRepositoryBean.findOne(DEFAULT_STORE_ID, DEFAULT_ATTRIBUTE_ID);
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        Mockito.verify(pcbFeign)
            .getAttributeDetailByAttributeId(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
                Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
                Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
                Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
                Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyString(), Mockito.anyBoolean());
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
      }
    }
  }

  @Test
  public void testFindOneWithNotNullUsername() throws Exception {
    AttributeResponse attribute = getAttribute();
    GdnRestSingleResponse<AttributeResponse> response =
        new GdnRestSingleResponse<AttributeResponse>(null, null, true, attribute, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    Mockito.when(pcbFeign.getAttributeDetailByAttributeId(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyString(), Mockito.anyBoolean()))
        .thenReturn(response);
    attributeRepositoryBean.findOne(DEFAULT_STORE_ID, DEFAULT_ATTRIBUTE_ID);
    Mockito.verify(pcbFeign).getAttributeDetailByAttributeId(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void findByStoreIdAndAttributeIdAndMarkForDeleteFalse() throws Exception {
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValueResponseList =
        new ArrayList<PredefinedAllowedAttributeValueResponse>();
    predefinedAllowedAttributeValueResponseList.add(predefinedAllowedAttributeValueResponse);
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response =
        new GdnRestListResponse<PredefinedAllowedAttributeValueResponse>(predefinedAllowedAttributeValueResponseList,
            PAGE_META_DATA, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
    Mockito.when(pcbFeign.getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(
        Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyInt(), Mockito.anyInt(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(response);
    attributeRepositoryBean.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(DEFAULT_REQUEST_ID, LIST_REQUEST,
        DEFAULT_ATTRIBUTE_ID, BRAND_VALUE);
    Mockito.verify(pcbFeign).getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(
        Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyInt(), Mockito.anyInt(),
        Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void findByStoreIdAndAttributeIdAndMarkForDeleteFalseWithException() throws Exception {
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response =
        new GdnRestListResponse<PredefinedAllowedAttributeValueResponse>("Read timeout",
            ErrorCategory.UNSPECIFIED.getCode(), false, DEFAULT_REQUEST_ID);
    Mockito.when(pcbFeign.getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(
        Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyInt(), Mockito.anyInt(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(response);
    try {
      attributeRepositoryBean.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(DEFAULT_REQUEST_ID, LIST_REQUEST,
          DEFAULT_ATTRIBUTE_ID, BRAND_VALUE);
    } catch (Exception e) {
    } finally {
      Mockito.verify(pcbFeign).getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(
          Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
          Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
          Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
          Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
          Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyInt(), Mockito.anyInt(),
          Mockito.anyString(), Mockito.anyString());
    }
  }
  
  @Test
  public void findByStoreIdAndAttributeIdAndMarkForDeleteFalseWithNotNullUsername() throws Exception {
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
        new PredefinedAllowedAttributeValueResponse();
    List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValueResponseList =
        new ArrayList<PredefinedAllowedAttributeValueResponse>();
    predefinedAllowedAttributeValueResponseList.add(predefinedAllowedAttributeValueResponse);
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response =
        new GdnRestListResponse<PredefinedAllowedAttributeValueResponse>(
            predefinedAllowedAttributeValueResponseList, PAGE_META_DATA, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    Mockito.when(pcbFeign.getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(
        Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyInt(), Mockito.anyInt(),
        Mockito.anyString(), Mockito.anyString())).thenReturn(response);
    attributeRepositoryBean.findByStoreIdAndAttributeIdAndMarkForDeleteFalse(
        DEFAULT_REQUEST_ID, LIST_REQUEST, DEFAULT_ATTRIBUTE_ID, BRAND_VALUE);
    Mockito.verify(pcbFeign).getPredefinedAllowedAttributeValueByAttributeIdAndValueAndPageable(
        Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyInt(), Mockito.anyInt(),
        Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void addNewAttributeTest() throws Exception {
    AttributeValueResponse attributeValueResponse = getAttributeValueResponse();
    GdnRestSingleResponse<AttributeValueResponse> response =
        new GdnRestSingleResponse<>(null, null, true, attributeValueResponse, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    Mockito.when(pcbFeign.addMasterAttributeValue(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyString(), Mockito.any()))
        .thenReturn(response);
    attributeRepositoryBean.addNewAttribute(DEFAULT_REQUEST_ID, ATTRIBUTE_VALUE, ATTRIBUTE_CODE);
    Mockito.verify(pcbFeign).addMasterAttributeValue(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
        Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void addNewAttributeWithException() throws Exception {
    GdnRestSingleResponse<AttributeValueResponse> response =
        new GdnRestSingleResponse<>("Read timeout", ErrorCategory.UNSPECIFIED.getCode(), false, null,
            DEFAULT_REQUEST_ID);
    Mockito.when(pcbFeign.addMasterAttributeValue(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyString(), Mockito.any()))
        .thenReturn(response);
    try {
      attributeRepositoryBean.addNewAttribute(DEFAULT_REQUEST_ID, ATTRIBUTE_VALUE, ATTRIBUTE_CODE);
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        Mockito.verify(pcbFeign).addMasterAttributeValue(Mockito.eq(GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(GdnMandatoryRequestParameterUtil.getUsername()), Mockito.anyString(), Mockito.any());
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
      }
    }
  }
}
