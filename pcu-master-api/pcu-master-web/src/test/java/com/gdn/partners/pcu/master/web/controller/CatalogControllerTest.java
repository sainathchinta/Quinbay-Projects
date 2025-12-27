package com.gdn.partners.pcu.master.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.model.CatalogApiPath;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.model.request.CategoryHierarchyServiceRequest;
import com.gdn.partners.pcu.master.model.request.GetSubCategoriesServiceRequest;
import com.gdn.partners.pcu.master.service.CatalogService;
import com.gdn.partners.pcu.master.web.helper.TestHelper;
import com.gdn.partners.pcu.master.web.model.response.CatalogDetailResponse;
import com.gdn.partners.pcu.master.web.model.response.CategorySearchWebResponse;
import com.gdn.partners.pcu.master.web.model.response.CategoryWebResponse;
import com.gdn.x.productcategorybase.dto.CatalogType;

@ExtendWith(MockitoExtension.class)
public class CatalogControllerTest extends TestHelper {
  private static final String CATALOG_ID = "catalog-id";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String DEFAULT_FILTER_TYPE = "ALL";
  private static final String CATEGORY_ID = "category-id";
  private static final String FILTER_TYPE = "filterType";
  private static final String CATEGORY_CODE = "CATAGORY_CODE";
  private static final String SEPARATOR = "/";
  private static final String HIDE_NON_INVENTORY= "hideNonInventory";
  private static final String BLANK_STRING = " ";
  private static final String DOCUMENT_FILTER_TYPE = "documentFilterType";
  private CategoryWebResponse categoryWebResponse;
  private List<CategoryWebResponse> categoryWebResponses;
  private List<CatalogDetailResponse> catalogDetailResponses;
  private CatalogDetailResponse catalogDetailResponse;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private CatalogService catalogService;

  @InjectMocks
  private CatalogController catalogController;

  @BeforeEach
  void setUp() {
    this.mockMvc = standaloneSetup(this.catalogController).build();
    categoryWebResponse = new CategoryWebResponse();
    categoryWebResponse.setCategoryCode(CATEGORY_CODE);
    categoryWebResponses = new ArrayList<>();
    categoryWebResponses.add(categoryWebResponse);
    catalogDetailResponse = new CatalogDetailResponse();
    catalogDetailResponse.setCatalogType(CatalogType.MASTER_CATALOG.name());
    catalogDetailResponse.setId(CATALOG_ID);
    catalogDetailResponses = new ArrayList<>();
    catalogDetailResponses.add(catalogDetailResponse);
  }

  @AfterEach
  void tearDown() throws Exception {
    verifyNoMoreInteractions(clientParameterHelper);
    verifyNoMoreInteractions(catalogService);
  }

  @Test
  void getCategorySummaryByCategoryIdTest() throws Exception {
    GetSubCategoriesServiceRequest
        request = new GetSubCategoriesServiceRequest(CatalogType.MASTER_CATALOG.name(),CATALOG_ID, CATEGORY_ID,
        Boolean.FALSE, Constants.ACTIVE, Constants.ALL, true, false);
    when(catalogService.getSubCategoriesByCatalogIdAndParentCategoryId(request)).thenReturn(categoryWebResponses);

    when(clientParameterHelper.getRequestId())
        .thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(CatalogApiPath.BASE_PATH + CatalogApiPath.GET_SUB_CATEGORIES_BY_CATEGORY_ID,
            CatalogType.MASTER_CATALOG.name(), CATALOG_ID, CATEGORY_ID)
            .param(HIDE_NON_INVENTORY, String.valueOf(Boolean.FALSE))
            .param(FILTER_TYPE, Constants.ACTIVE)
            .param(DOCUMENT_FILTER_TYPE, Constants.ALL)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());

    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(clientParameterHelper).getRequestId();
    verify(catalogService).getSubCategoriesByCatalogIdAndParentCategoryId(request);
  }

  @Test
  void getCategorySummaryByCategoryIdEmptyCatalogIdTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(CatalogApiPath.BASE_PATH + CatalogApiPath.GET_SUB_CATEGORIES_BY_CATEGORY_ID,
            CatalogType.MASTER_CATALOG.name(), BLANK_STRING, CATEGORY_ID).param(HIDE_NON_INVENTORY,
                String.valueOf(Boolean.FALSE)).param(FILTER_TYPE, Constants.ACTIVE)
            .param(DOCUMENT_FILTER_TYPE, Constants.ALL).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(clientParameterHelper).getRequestId();

  }

  @Test
  void getListOfCategoryHierarchyByCategoryNameTest() throws Exception{
    List<List<CategorySearchWebResponse>> response = new ArrayList<>();
    CategorySearchWebResponse categorySearchWebResponse = new CategorySearchWebResponse();
    categorySearchWebResponse.setCategoryCode(CATEGORY_CODE);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(catalogService.getListOfCategoryHierarchyByCategoryName(
        new CategoryHierarchyServiceRequest(CATEGORY_NAME, CATALOG_ID, DEFAULT_FILTER_TYPE, DEFAULT_FILTER_TYPE)))
        .thenReturn(response);

    MockHttpServletRequestBuilder requestBuilder =
        get(CatalogApiPath.BASE_PATH + CatalogApiPath.FILTER_BY_CATALOG_ID_AND_CATEGORY_NAME_AND_STATE, CATALOG_ID)
            .param(CATEGORY_NAME, CATEGORY_NAME)
            .param(FILTER_TYPE, DEFAULT_FILTER_TYPE)
            .param(DOCUMENT_FILTER_TYPE, DEFAULT_FILTER_TYPE)
            .contentType(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(clientParameterHelper).getRequestId();
    verify(catalogService).getListOfCategoryHierarchyByCategoryName(
        new CategoryHierarchyServiceRequest(CATEGORY_NAME, CATALOG_ID, DEFAULT_FILTER_TYPE, DEFAULT_FILTER_TYPE));
  }

  @Test
  void getListOfCategoryHierarchyByCategoryName_EmptyParameterTest() throws Exception{
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);

    MockHttpServletRequestBuilder requestBuilder =
        get(CatalogApiPath.BASE_PATH + CatalogApiPath.FILTER_BY_CATALOG_ID_AND_CATEGORY_NAME_AND_STATE, CATALOG_ID)
            .param(CATEGORY_NAME, StringUtils.EMPTY)
            .param(FILTER_TYPE, DEFAULT_FILTER_TYPE)
            .param(DOCUMENT_FILTER_TYPE, DEFAULT_FILTER_TYPE)
            .contentType(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));

    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getCategorySummaryByCategoryIdEmptyCatalogTypeTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(CatalogApiPath.BASE_PATH + CatalogApiPath.GET_SUB_CATEGORIES_BY_CATEGORY_ID, BLANK_STRING, CATALOG_ID,
            CATEGORY_ID).param(HIDE_NON_INVENTORY, String.valueOf(Boolean.FALSE))
            .param(FILTER_TYPE, Constants.ACTIVE).param(DOCUMENT_FILTER_TYPE, Constants.ALL).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    verify(clientParameterHelper).getRequestId();

  }

  @Test
  void getCategorySummaryByCategoryIdEmptyParentIdTest() throws Exception {
    GetSubCategoriesServiceRequest
        request = new GetSubCategoriesServiceRequest(CatalogType.MASTER_CATALOG.name(),CATALOG_ID, null,
        Boolean.FALSE, Constants.ACTIVE, Constants.ALL, true, false);
    when(catalogService.getSubCategoriesByCatalogIdAndParentCategoryId(request)).thenReturn(categoryWebResponses);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(CatalogApiPath.BASE_PATH + CatalogApiPath.GET_SUB_CATEGORIES_BY_CATEGORY_ID, CatalogType.MASTER_CATALOG.name(), CATALOG_ID,
            BLANK_STRING).param(HIDE_NON_INVENTORY, String.valueOf(Boolean.FALSE))
            .param(FILTER_TYPE, Constants.ACTIVE)
            .param(DOCUMENT_FILTER_TYPE, Constants.ALL).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(catalogService).getSubCategoriesByCatalogIdAndParentCategoryId(request);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void getCatalogSummaryByCatalogType() throws Exception {
    when(catalogService.getCatalogSummaryByCatalogType(CatalogType.MASTER_CATALOG.name()))
        .thenReturn(catalogDetailResponses);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(CatalogApiPath.BASE_PATH + SEPARATOR + CatalogApiPath.GET_CATALOG_INFO_BY_CATALOG_TYPE, CatalogType.MASTER_CATALOG.name())
            .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(catalogService).getCatalogSummaryByCatalogType(CatalogType.MASTER_CATALOG.name());
    verify(clientParameterHelper).getRequestId();
  }

}