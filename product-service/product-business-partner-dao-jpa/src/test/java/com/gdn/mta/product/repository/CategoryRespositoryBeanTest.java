package com.gdn.mta.product.repository;

import static org.mockito.ArgumentMatchers.eq;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.slf4j.MDC;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.x.productcategorybase.dto.CatalogType;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;

public class CategoryRespositoryBeanTest {

  private static final VerificationMode AT_LEAST_ONE = Mockito.times(1);
  private static final String DEFAULT_CATEGORY_CODE = "CAT-0000001";
  private static final String DEFAULT_CATEGORY_CODE_2 = "CAT-0000002";
  private final String USERNAME = "username";
  private final String CATEGORY_ID = "OL-1234";
  private static final String REQUEST_ID = "request id";

  @InjectMocks
  private CategoryRepositoryBean categoryRepositoryBean;

  @Mock
  private PCBFeign pcbFeign;

  @Captor
  private ArgumentCaptor<CategoryCodeRequest> codeRequestArgumentCaptor;

  private String requestId;

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.pcbFeign);
  }

  public String getRequestId() {
    return requestId;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    setRequestId(UUID.randomUUID().toString());

    List<CategoryResponse> categories = new ArrayList<CategoryResponse>();
    categories.add(new CategoryResponse());
    GdnRestListResponse<CategoryResponse> categoryHierarchy =
        new GdnRestListResponse<CategoryResponse>(null, null, true, categories, new PageMetaData(
            10, 0, 1), getRequestId());
    GdnRestListResponse<CategoryResponse> categoryHierarchyError =
        new GdnRestListResponse<CategoryResponse>("Read Timeout",
            ErrorCategory.UNSPECIFIED.getCode(), false, null, null, getRequestId());

    Mockito.when(this.pcbFeign.filterCategoryHierarchyByCategoryCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        DEFAULT_CATEGORY_CODE)).thenReturn(categoryHierarchy);
    Mockito.when(this.pcbFeign.filterCategoryHierarchyByCategoryCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        DEFAULT_CATEGORY_CODE_2)).thenReturn(categoryHierarchyError);
  }

  public void setCategoryRepositoryBean(CategoryRepositoryBean categoryRepositoryBean) {
    this.categoryRepositoryBean = categoryRepositoryBean;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  @Test
  public void testFindOne() throws Exception {
    CategoryDetailResponse categoryDetailResponse =
        new CategoryDetailResponse(new CategoryResponse("Kategori 1", "CAT-0000001", 0, null, null,
            null, null, true, 0, false, false, true, null, null));
    categoryDetailResponse.setId(UUID.randomUUID().toString());
    categoryDetailResponse.setCatalog(new CatalogResponse("Master Katalog", null,
        CatalogType.MASTER_CATALOG.toString()));
    GdnRestSingleResponse<CategoryDetailResponse> response =
        new GdnRestSingleResponse<CategoryDetailResponse>(null, null, true, categoryDetailResponse,
            getRequestId());
    Mockito.when(
        this.pcbFeign.getCategoryDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            categoryDetailResponse.getId())).thenReturn(response);
    categoryRepositoryBean.findOne(categoryDetailResponse.getId());
    Mockito.verify(this.pcbFeign, AT_LEAST_ONE)
        .getCategoryDetail(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            categoryDetailResponse.getId());
  }

  @Test
  public void testFindHierarchyByCategoryCode() throws Exception {
    categoryRepositoryBean.findHierarchyByCategoryCode(DEFAULT_CATEGORY_CODE);
    Mockito.verify(pcbFeign).filterCategoryHierarchyByCategoryCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        DEFAULT_CATEGORY_CODE);
  }

  @Test
  public void testFindHierarchyByCategoryCodes() throws Exception {
    GdnRestListResponse<CategoryHierarchyResponse> response = new GdnRestListResponse<>(null, null, true, "");
    Mockito.when(
        pcbFeign.filterCategoryHierarchyByCategoryCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any())).thenReturn(response);
    categoryRepositoryBean.findHierarchyByCategoryCodes(new CategoryCodeRequest());
    Mockito.verify(pcbFeign).filterCategoryHierarchyByCategoryCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void testFindHierarchyByCategoryCodesException() throws Exception {
    GdnRestListResponse<CategoryHierarchyResponse> response = new GdnRestListResponse<>(null, null, false, "");
    Mockito.when(
            pcbFeign.filterCategoryHierarchyByCategoryCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), Mockito.anyString(), Mockito.any()))
        .thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        categoryRepositoryBean.findHierarchyByCategoryCodes(new CategoryCodeRequest());
      });
    } catch (Exception e) {
      throw e;
    }
    Mockito.verify(pcbFeign)
        .filterCategoryHierarchyByCategoryCodes(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void testFindHierarchyByCategoryCodeWithMDCValue() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, getRequestId());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        GdnBaseLookup.DEFAULT_USERNAME);
    List<CategoryResponse> categories = new ArrayList<CategoryResponse>();
    categories.add(new CategoryResponse());
    GdnRestListResponse<CategoryResponse> categoryHierarchy =
        new GdnRestListResponse<CategoryResponse>(null, null, true, categories, new PageMetaData(
            10, 0, 1), getRequestId());
    Mockito.when(this.pcbFeign.filterCategoryHierarchyByCategoryCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        DEFAULT_CATEGORY_CODE)).thenReturn(categoryHierarchy);
    categoryRepositoryBean.findHierarchyByCategoryCode(DEFAULT_CATEGORY_CODE);
    Mockito.verify(pcbFeign).filterCategoryHierarchyByCategoryCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        DEFAULT_CATEGORY_CODE);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, null);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, null);
  }

  @Test
  public void testFindHierarchyByCategoryCodeWithError() throws Exception {
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        categoryRepositoryBean.findHierarchyByCategoryCode(DEFAULT_CATEGORY_CODE_2);
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
    Mockito.verify(pcbFeign).filterCategoryHierarchyByCategoryCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        DEFAULT_CATEGORY_CODE_2);
  }

  @Test
  public void getFinalParentCategoryCached() throws Exception{
    GdnRestSingleResponse<SingleObjectResponse> response =
        new GdnRestSingleResponse<SingleObjectResponse>(null, null, true, null, requestId);
    Mockito.when(pcbFeign.getFinalParentCategoryCached(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), requestId,
        USERNAME, CATEGORY_ID)).thenReturn(response);

    categoryRepositoryBean.getFinalParentCategoryCached(requestId, USERNAME, CATEGORY_ID);

    Mockito.verify(pcbFeign).getFinalParentCategoryCached(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), requestId,
        USERNAME, CATEGORY_ID);
  }

  @Test
  public void getFinalParentCategoryCachedWhenFalse() throws Exception{
    GdnRestSingleResponse<SingleObjectResponse> response =
        new GdnRestSingleResponse<SingleObjectResponse>(null, null, false, null, requestId);
    Mockito.when(pcbFeign.getFinalParentCategoryCached(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), requestId,
        USERNAME, CATEGORY_ID)).thenReturn(response);

    try{
      Assertions.assertThrows(Exception.class, () -> {
        categoryRepositoryBean.getFinalParentCategoryCached(requestId, USERNAME, CATEGORY_ID);
      });
    } catch(Exception e){
      throw e;
    }
    Mockito.verify(pcbFeign).getFinalParentCategoryCached(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), requestId,
        USERNAME, CATEGORY_ID);
  }

  @Test
  public void getFinalParentCategoryCachedWhenError() throws Exception{
    Mockito.when(pcbFeign.getFinalParentCategoryCached(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), requestId,
        USERNAME, CATEGORY_ID)).thenThrow(RuntimeException.class);
    try{
      Assertions.assertThrows(Exception.class, () -> {
        categoryRepositoryBean.getFinalParentCategoryCached(requestId, USERNAME, CATEGORY_ID);
      });
    } catch(Exception e){
      throw e;
    }
    Mockito.verify(pcbFeign).getFinalParentCategoryCached(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), requestId,
        USERNAME, CATEGORY_ID);
  }

  @Test
  public void getAllChildCategoriesByC1CategoryCode() throws Exception {
    GdnRestSingleResponse<CategoryCodeResponse> response =
        new GdnRestSingleResponse<>(null, null, false, null, requestId);
    CategoryCodeRequest categoryCodeRequest = new CategoryCodeRequest();
    categoryCodeRequest.setCategoryCodes(Arrays.asList(CATEGORY_ID));
    Mockito.when(pcbFeign.getAllChildCategoriesFromC1CategoryCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), REQUEST_ID,
        USERNAME, false, categoryCodeRequest)).thenReturn(response);

    try {
      categoryRepositoryBean.getAllChildCategoriesByC1CategoryCode(REQUEST_ID, USERNAME, CATEGORY_ID);
    } catch (Exception e) {
    } finally {
      Mockito.verify(pcbFeign).getAllChildCategoriesFromC1CategoryCode(eq(GdnMandatoryRequestParameterUtil.getStoreId()),
          eq(GdnMandatoryRequestParameterUtil.getChannelId()), eq(GdnMandatoryRequestParameterUtil.getClientId()),
          eq(REQUEST_ID), eq(USERNAME), eq(false), codeRequestArgumentCaptor.capture());
    }

  }
}
