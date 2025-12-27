package com.gdn.mta.product.repository;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.slf4j.MDC;
import com.gdn.mta.product.util.BeanUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.entity.Attribute;

public class AttributeRepositoryBeanTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final VerificationMode AT_LEAST_ONE = Mockito.times(1);
  private static final String DEFAULT_ATTRIBUTE_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_ATTRIBUTE_ID_2 = UUID.randomUUID().toString();
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();

  @InjectMocks
  private AttributeRepositoryBean attributeRepositoryBean;

  @Mock
  private PCBFeign pcbFeign;

  private String requestId;

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.pcbFeign);
  }

  private Attribute getAttribute() {
    Attribute attribute = new Attribute("Attribute 1", AttributeType.DESCRIPTIVE_ATTRIBUTE, false, DEFAULT_STORE_ID);
    attribute.setId(UUID.randomUUID().toString());
    return attribute;
  }

  public String getRequestId() {
    return requestId;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    setRequestId(UUID.randomUUID().toString());
    AttributeResponse attributeResponseData = new AttributeResponse();
    GdnRestSingleResponse<AttributeResponse> responseDetail =
        new GdnRestSingleResponse<AttributeResponse>(attributeResponseData,
            AttributeRepositoryBeanTest.DEFAULT_REQUEST_ID);
    GdnRestSingleResponse<AttributeResponse> responseDetailError =
        new GdnRestSingleResponse<AttributeResponse>("Read Timeout",
            ErrorCategory.UNSPECIFIED.getCode(), false, null,
            AttributeRepositoryBeanTest.DEFAULT_REQUEST_ID);
    Mockito.when(this.pcbFeign.getAttributeDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        AttributeRepositoryBeanTest.DEFAULT_ATTRIBUTE_ID, false)).thenReturn(responseDetail);
    Mockito.when(this.pcbFeign.getAttributeDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        AttributeRepositoryBeanTest.DEFAULT_ATTRIBUTE_ID_2, false)).thenReturn(responseDetailError);
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  @Test
  public void testFindOne() throws Exception {
    Attribute attribute = getAttribute();
    AttributeResponse attributeResponse = new AttributeResponse();
    BeanUtils.copyProperties(attribute, attributeResponse);
    GdnRestSingleResponse<AttributeResponse> response =
        new GdnRestSingleResponse<AttributeResponse>(null, null, true, attributeResponse, getRequestId());
    Mockito.when(this.pcbFeign.getAttributeDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        attribute.getId(), false)).thenReturn(response);
    attributeRepositoryBean.findOne(attribute.getId());
    Mockito.verify(this.pcbFeign).getAttributeDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        attribute.getId(), false);

  }

  @Test
  public void testFindOneResponseWithAllowedAttributeValuesNull() throws Exception {
    Attribute attribute = getAttribute();
    AttributeResponse attributeResponse = new AttributeResponse();
    BeanUtils.copyProperties(attribute, attributeResponse);
    attributeResponse.setAllowedAttributeValues(null);
    attributeResponse.setPredefinedAllowedAttributeValues(null);
    GdnRestSingleResponse<AttributeResponse> response =
        new GdnRestSingleResponse<AttributeResponse>(null, null, true, attributeResponse, getRequestId());
    Mockito.when(this.pcbFeign.getAttributeDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        attribute.getId(), false)).thenReturn(response);
    attributeRepositoryBean.findOne(attribute.getId());
    Mockito.verify(this.pcbFeign).getAttributeDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        attribute.getId(), false);

  }


  @Test
  public void testFindOneResponseWithAllowedAttributeValues() throws Exception {
    Attribute attribute = getAttribute();
    AttributeResponse attributeResponse = new AttributeResponse();
    BeanUtils.copyProperties(attribute, attributeResponse);

    List<AllowedAttributeValueResponse> allowedAttributeValueResponseList = new ArrayList<>();
    AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
    allowedAttributeValueResponse.setId(DEFAULT_ATTRIBUTE_ID);
    allowedAttributeValueResponseList.add(allowedAttributeValueResponse);
    attributeResponse.setAllowedAttributeValues(allowedAttributeValueResponseList);

    List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValueResponseList = new ArrayList<>();
    PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse = new PredefinedAllowedAttributeValueResponse();
    predefinedAllowedAttributeValueResponse.setId(DEFAULT_ATTRIBUTE_ID);
    predefinedAllowedAttributeValueResponseList.add(predefinedAllowedAttributeValueResponse);
    attributeResponse.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValueResponseList);

    GdnRestSingleResponse<AttributeResponse> response =
        new GdnRestSingleResponse<AttributeResponse>(null, null, true, attributeResponse, getRequestId());
    Mockito.when(this.pcbFeign.getAttributeDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        attribute.getId(), false)).thenReturn(response);
    attributeRepositoryBean.findOne(attribute.getId());
    Mockito.verify(this.pcbFeign).getAttributeDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        attribute.getId(), false);

  }

  @Test
  public void testFindOneAttributeResponseNullTest() throws Exception {
    Attribute attribute = getAttribute();
    attribute.setId(DEFAULT_ATTRIBUTE_ID);
    try{
      GdnRestSingleResponse<AttributeResponse> response =
          new GdnRestSingleResponse<AttributeResponse>(null, null, true, null, getRequestId());
      Mockito.when(this.pcbFeign.getAttributeDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
          attribute.getId(), false)).thenReturn(response);
      Assertions.assertThrows(ApplicationException.class, () -> {
        attributeRepositoryBean.findOne(attribute.getId());
      });

    } finally {
      Mockito.verify(this.pcbFeign).getAttributeDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
          attribute.getId(), false);
    }

  }

  @Test
  public void findDetailByIdTest() throws Exception {
    this.attributeRepositoryBean.findDetailById(AttributeRepositoryBeanTest.DEFAULT_ATTRIBUTE_ID);
    Mockito.verify(this.pcbFeign).getAttributeDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        AttributeRepositoryBeanTest.DEFAULT_ATTRIBUTE_ID, false);
  }

  @Test
  public void findDetailByIdWithErrorTest() throws Exception {
    try {
      MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
          AttributeRepositoryBeanTest.DEFAULT_REQUEST_ID);
      MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
          GdnBaseLookup.DEFAULT_USERNAME);
      GdnRestSingleResponse<AttributeResponse> response =
          new GdnRestSingleResponse<AttributeResponse>(null, null, false, null, null);
      Mockito.when(pcbFeign.getAttributeDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
          GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
          GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
          AttributeRepositoryBeanTest.DEFAULT_ATTRIBUTE_ID_2, false)).thenReturn(response);
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.attributeRepositoryBean.findDetailById(AttributeRepositoryBeanTest.DEFAULT_ATTRIBUTE_ID_2);
      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.pcbFeign).getAttributeDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        AttributeRepositoryBeanTest.DEFAULT_ATTRIBUTE_ID_2, false);
  }
  
}
