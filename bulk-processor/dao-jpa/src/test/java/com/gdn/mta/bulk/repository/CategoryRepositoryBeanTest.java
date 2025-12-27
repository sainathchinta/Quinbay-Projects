package com.gdn.mta.bulk.repository;


import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.bulk.util.Constant;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.ListHolderRequest;
import com.gdn.mta.bulk.dto.product.AllowedAttributeValueDtoRequest;
import com.gdn.mta.bulk.dto.product.AllowedAttributeValueDtoResponse;
import com.gdn.mta.bulk.dto.product.AllowedValueDtoResponse;
import com.gdn.mta.bulk.feignConfig.PCBFeign;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;

public class CategoryRepositoryBeanTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_USERNAME = "testing@mail.com";
  private static final String DEFAULT_CATEGORY_CODE = "CAT-0000001";
  private static final String DEFAULT_ATTRIBUTE_CODE = "BR-3433";
  private static final String DEFAULT_ATTRIBUTE_TYPE = "PREDEFINED";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String ROM = "32GB";
  private static final String BRAND_NAME = "Samsung";

  private List<AllowedAttributeValueDtoRequest> allowedAttributeValueDtoRequests;
  private List<AllowedAttributeValueDtoResponse> allowedAttributeValueDtoResponses;
  private GdnRestListResponse<AllowedAttributeValueDtoResponse> allowedAttributeValueDtoResponseGdnRestListResponse;
  private List<String> attributeValues;
  private List<AllowedValueDtoResponse> allowedValueDtoResponses;

  
  @Mock
  private PCBFeign pcbFeign;

  @InjectMocks
  private CategoryRepositoryBean categoryRepositoryBean;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    attributeValues = Collections.singletonList(ROM);
    allowedAttributeValueDtoRequests = Collections
        .singletonList(new AllowedAttributeValueDtoRequest(DEFAULT_ATTRIBUTE_CODE, DEFAULT_ATTRIBUTE_TYPE,
            attributeValues));
    allowedValueDtoResponses = Collections.singletonList(new AllowedValueDtoResponse());
    allowedAttributeValueDtoResponses = Collections.singletonList(
        new AllowedAttributeValueDtoResponse(DEFAULT_ATTRIBUTE_CODE, DEFAULT_ATTRIBUTE_TYPE, allowedValueDtoResponses));
    allowedAttributeValueDtoResponseGdnRestListResponse =
        new GdnRestListResponse<>(allowedAttributeValueDtoResponses, new PageMetaData(20, 0, 20), DEFAULT_REQUEST_ID);
    allowedAttributeValueDtoResponseGdnRestListResponse.setSuccess(true);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, "api");
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, "x-bulk");
  }

  @Test
  public void validateIsCnCategory_NotValid_ThrowsException() throws Exception {
    try {
      when(pcbFeign.validateIsCnCategory(anyString(), anyString(), anyString(), anyString(),
          anyString(), anyString()))
          .thenThrow(new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, "Unspecified error"));
      categoryRepositoryBean.validateIsCnCategory(DEFAULT_STORE_ID, DEFAULT_CATEGORY_CODE);
    } catch (Exception e) {
      if (e instanceof ApplicationRuntimeException) {
        verify(pcbFeign).validateIsCnCategory(anyString(), anyString(), anyString(), anyString(),
            anyString(), anyString());
        ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e;
        Assertions.assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
      }
    }
  }

  @Test
  public void validateIsCnCategory_UsingMDCValue_ThrowsException() throws Exception {
    try {
      MDC.put("username", DEFAULT_USERNAME);
      when(pcbFeign.validateIsCnCategory(anyString(), anyString(), anyString(), anyString(),
          anyString(), anyString()))
          .thenThrow(new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, "Unspecified error"));
      categoryRepositoryBean.validateIsCnCategory(DEFAULT_STORE_ID, DEFAULT_CATEGORY_CODE);
    } catch (Exception e) {
      if (e instanceof ApplicationRuntimeException) {
        verify(pcbFeign).validateIsCnCategory(anyString(), anyString(), anyString(), anyString(),
            anyString(), anyString());
        ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e;
        Assertions.assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
      }
    }
  }

  @Test
  public void getPredefinedAndDefiningAllowedAttributeValueTest() throws Exception {
    ListHolderRequest<AllowedAttributeValueDtoRequest> request =
        new ListHolderRequest<>(allowedAttributeValueDtoRequests);
    Mockito.when(pcbFeign.getPredefinedAndDefiningAllowedAttributeValue(eq(DEFAULT_STORE_ID),
        eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID), eq(DEFAULT_REQUEST_ID), eq(DEFAULT_USERNAME),
        Mockito.any())).thenReturn(allowedAttributeValueDtoResponseGdnRestListResponse);
    List<AllowedAttributeValueDtoResponse> ans =
        categoryRepositoryBean.getPredefinedAndDefiningAllowedAttributeValue(DEFAULT_USERNAME,
            DEFAULT_REQUEST_ID, DEFAULT_STORE_ID, allowedAttributeValueDtoRequests);
    Assertions.assertEquals(DEFAULT_ATTRIBUTE_CODE, ans.get(0).getAttributeCode());
    Assertions.assertEquals(DEFAULT_ATTRIBUTE_TYPE, ans.get(0).getAttributeType());
    verify(pcbFeign).getPredefinedAndDefiningAllowedAttributeValue(eq(DEFAULT_STORE_ID),
        eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID), eq(DEFAULT_REQUEST_ID), eq(DEFAULT_USERNAME),
        Mockito.any());
  }

  @Test
  public void getPredefinedAndDefiningAllowedAttributeValueTestWithException() throws Exception {
    Mockito.when(
        pcbFeign.getPredefinedAndDefiningAllowedAttributeValue(eq(DEFAULT_STORE_ID),
            eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID), eq(DEFAULT_REQUEST_ID), eq(DEFAULT_USERNAME),
            Mockito.any()))
        .thenThrow(ApplicationRuntimeException.class);
    try {
      categoryRepositoryBean
          .getPredefinedAndDefiningAllowedAttributeValue(DEFAULT_USERNAME, DEFAULT_REQUEST_ID, DEFAULT_STORE_ID,
              allowedAttributeValueDtoRequests);
    } catch (ApplicationRuntimeException e) {
    } finally {
      verify(pcbFeign).getPredefinedAndDefiningAllowedAttributeValue(eq(DEFAULT_STORE_ID),
          eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID), eq(DEFAULT_REQUEST_ID), eq(DEFAULT_USERNAME),
          Mockito.any());
    }
  }

  @Test
  public void getPredefinedAndDefiningAllowedAttributeValueTestWithNullPointerException() throws Exception {
    try {
      categoryRepositoryBean
          .getPredefinedAndDefiningAllowedAttributeValue(DEFAULT_USERNAME, DEFAULT_REQUEST_ID, DEFAULT_STORE_ID,
              allowedAttributeValueDtoRequests);
    } catch (NullPointerException e) {
    } finally {
      verify(pcbFeign).getPredefinedAndDefiningAllowedAttributeValue(eq(DEFAULT_STORE_ID),
          eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID), eq(DEFAULT_REQUEST_ID), eq(DEFAULT_USERNAME),
          Mockito.any());
    }
  }

  @Test
  public void getPredefinedAndDefiningAllowedAttributeValueTestWithApplicationException() throws Exception {
    GdnRestListResponse gdnRestListResponse = new GdnRestListResponse<>();
    gdnRestListResponse.setSuccess(false);
    String s = "String";
    Mockito.when(
        pcbFeign.getPredefinedAndDefiningAllowedAttributeValue(eq(DEFAULT_STORE_ID),
            eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID), eq(DEFAULT_REQUEST_ID), eq(DEFAULT_USERNAME),
            Mockito.any())).thenReturn(gdnRestListResponse);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.categoryRepositoryBean.getPredefinedAndDefiningAllowedAttributeValue(
              DEFAULT_USERNAME, DEFAULT_REQUEST_ID, DEFAULT_STORE_ID,
              allowedAttributeValueDtoRequests));
    }
    finally {
      verify(pcbFeign).getPredefinedAndDefiningAllowedAttributeValue(eq(DEFAULT_STORE_ID),
          eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID), eq(DEFAULT_REQUEST_ID), eq(DEFAULT_USERNAME),
          Mockito.any());
    }
  }

  @Test
  public void testFindByStoreIdAndCategoryCodeAndMarkDeleteFalseWithException() throws Exception {
    try {
      when(pcbFeign.getCategoryDetailByCode(anyString(), anyString(), anyString(), anyString(),
          anyString(), anyString())).thenThrow(
          new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, "Unspecified error"));
      categoryRepositoryBean.findByStoreIdAndCategoryCodeAndMarkDeleteFalse(DEFAULT_STORE_ID, DEFAULT_CATEGORY_CODE);
    } catch (Exception e) {
      if (e instanceof ApplicationRuntimeException) {
        verify(pcbFeign).getCategoryDetailByCode(anyString(), anyString(), anyString(), anyString(), anyString(),
            anyString());
        ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e;
        Assertions.assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
      }
    }
  }

  @Test
  public void testfilterCategoryHierarchyByCategoryCode() throws Exception {
    try {
      MDC.put("username", DEFAULT_USERNAME);
      when(pcbFeign.filterCategoryHierarchyByCategoryCode(anyString(), anyString(), anyString(), anyString(),
          anyString(), anyString())).thenThrow(new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, "Unspecified error"));
      categoryRepositoryBean.filterCategoryHierarchyByCategoryCode(DEFAULT_STORE_ID, DEFAULT_CATEGORY_CODE);
    } catch (Exception e) {
      if (e instanceof ApplicationRuntimeException) {
        verify(pcbFeign).filterCategoryHierarchyByCategoryCode(anyString(), anyString(),
            anyString(), anyString(), anyString(), anyString());
        ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e;
        Assertions.assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
      }
    }
  }

  @Test
  public void getBrandDetail_HappyFlow_Success() throws Exception {
    Mockito.when(
        pcbFeign.filterByBrandName(Mockito.eq(com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil.getUsername()), Mockito.eq(BRAND_NAME),
            Mockito.eq(false), Mockito.eq(false))).thenReturn(this.generateBrandDetailResponse());
    BrandResponse brandDetail = categoryRepositoryBean.getBrandDetail(DEFAULT_USERNAME, DEFAULT_REQUEST_ID, BRAND_NAME);
    Mockito.verify(pcbFeign)
        .filterByBrandName(Mockito.eq(com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil.getStoreId()),
            Mockito.eq(com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil.getChannelId()),
            Mockito.eq(com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil.getClientId()),
            Mockito.eq(com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil.getRequestId()),
            Mockito.eq(com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil.getUsername()), Mockito.eq(BRAND_NAME),
            Mockito.eq(false), Mockito.eq(false));
    Assertions.assertEquals(BRAND_NAME, brandDetail.getBrandName());
  }

  private GdnRestSingleResponse<BrandResponse> generateBrandDetailResponse() {
    BrandResponse brandDetail =  new BrandResponse();
    brandDetail.setBrandName(BRAND_NAME);
    return new GdnRestSingleResponse<BrandResponse>(brandDetail, DEFAULT_REQUEST_ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(pcbFeign);
  }
}
