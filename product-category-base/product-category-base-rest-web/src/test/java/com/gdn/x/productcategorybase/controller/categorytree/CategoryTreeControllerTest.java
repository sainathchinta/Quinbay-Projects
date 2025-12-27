package com.gdn.x.productcategorybase.controller.categorytree;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.dto.categorytree.CategoryNodeFilterCategoryCodesRequest;
import com.gdn.x.productcategorybase.dto.categorytree.CategoryNodeFilterParentCategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.categorytree.CategoryTreeFilterCategoryCodesRequest;
import com.gdn.x.productcategorybase.entity.categorytree.CategoryNode;
import com.gdn.x.productcategorybase.service.categorytree.CategoryTreeService;

public class CategoryTreeControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "API";
  private static final String DEFAULT_CLIENT_ID = "TEST";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_USERNAME = "DEVELOPER";
  private static final String DEFAULT_CATALOG_CODE = "10001";
  private static final String DEFAULT_CATEGORY_CODE = "CAT-0000001";
  private static final String DEFAULT_CATEGORY_CODE_2 = "CAT-0000002";
  private static final String DEFAULT_CATEGORY_CODE_3 = "CAT-0000003";
  private static final VerificationMode NEVER_CALLED = Mockito.times(0);

  @Mock
  private CategoryTreeService categoryTreeService;

  @InjectMocks
  private CategoryTreeController categoryTreeController;

  private MockMvc mockMvc;
  private ObjectMapper objectMapper;

  @SuppressWarnings("unchecked")
  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc =
        MockMvcBuilders
            .standaloneSetup(this.categoryTreeController)
            .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
                new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
                new MappingJackson2HttpMessageConverter()).build();
    this.objectMapper = new ObjectMapper(new JsonFactory());
    List<CategoryNode> categoryNodes = this.generateCategoryNodes();
    Mockito.when(
        this.categoryTreeService.findCategoryNodeByCatalogCodeAndCategoryCodesAndActive(Mockito.anyString(),
            Mockito.anyList(), Mockito.anyBoolean())).thenReturn(categoryNodes);
    Mockito.when(
        this.categoryTreeService.findCategoryNodeByCatalogCodeAndParentCategoryCodeAndActive(Mockito.anyString(),
            Mockito.anyString(), Mockito.anyBoolean())).thenReturn(categoryNodes);
    Mockito.when(
        this.categoryTreeService.findByCatalogCodeAndCategoryCodesAndActive(Mockito.anyString(), Mockito.anyList(),
            Mockito.anyBoolean())).thenReturn(categoryNodes);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.categoryTreeService);
  }

  private CategoryNode generateCategoryNode() throws Exception {
    CategoryNode categoryNode = new CategoryNode();
    categoryNode.setCategoryCode(CategoryTreeControllerTest.DEFAULT_CATEGORY_CODE);
    categoryNode.setChildCount(0L);
    return categoryNode;
  }

  private List<CategoryNode> generateCategoryNodes() throws Exception {
    List<CategoryNode> categoryNodes = new ArrayList<CategoryNode>();
    categoryNodes.add(this.generateCategoryNode());
    categoryNodes.add(this.generateCategoryNode());
    categoryNodes.add(this.generateCategoryNode());
    categoryNodes.get(0).setChildCount(1L);
    categoryNodes.get(1).setParentCategoryCode(CategoryTreeControllerTest.DEFAULT_CATEGORY_CODE);
    categoryNodes.get(1).setCategoryCode(CategoryTreeControllerTest.DEFAULT_CATEGORY_CODE_2);
    categoryNodes.get(1).setChildCount(1L);
    categoryNodes.get(2).setParentCategoryCode(CategoryTreeControllerTest.DEFAULT_CATEGORY_CODE_2);
    categoryNodes.get(2).setCategoryCode(CategoryTreeControllerTest.DEFAULT_CATEGORY_CODE_3);
    categoryNodes.get(2).setChildCount(0L);
    return categoryNodes;
  }

  private CategoryNodeFilterCategoryCodesRequest generateCategoryNodeFilterCategoryCodesRequest() throws Exception {
    CategoryNodeFilterCategoryCodesRequest request = new CategoryNodeFilterCategoryCodesRequest();
    request.setCatalogCode(CategoryTreeControllerTest.DEFAULT_CATALOG_CODE);
    request.setCategoryCodes(new ArrayList<String>());
    request.getCategoryCodes().add(CategoryTreeControllerTest.DEFAULT_CATEGORY_CODE);
    return request;
  }

  private CategoryNodeFilterParentCategoryCodeRequest generateCategoryNodeFilterParentCategoryCodeRequest()
      throws Exception {
    CategoryNodeFilterParentCategoryCodeRequest request = new CategoryNodeFilterParentCategoryCodeRequest();
    request.setCatalogCode(CategoryTreeControllerTest.DEFAULT_CATALOG_CODE);
    request.setParentCategoryCode(CategoryTreeControllerTest.DEFAULT_CATEGORY_CODE);
    return request;
  }

  private CategoryTreeFilterCategoryCodesRequest generateCategoryTreeFilterCategoryCodesRequest() throws Exception {
    CategoryTreeFilterCategoryCodesRequest request = new CategoryTreeFilterCategoryCodesRequest();
    request.setCatalogCode(CategoryTreeControllerTest.DEFAULT_CATALOG_CODE);
    request.setCategoryCodes(new ArrayList<String>());
    request.getCategoryCodes().add(CategoryTreeControllerTest.DEFAULT_CATEGORY_CODE);
    request.getCategoryCodes().add(CategoryTreeControllerTest.DEFAULT_CATEGORY_CODE_2);
    request.setBuildTree(true);
    return request;
  }

  @SuppressWarnings("unchecked")
  @Test
  public void filterCategoryNodeByCategoryCodesTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(CategoryTreeControllerPath.BASE_PATH + CategoryTreeControllerPath.FILTER_NODE_CATEGORY_CODES)
            .addParameter("storeId", CategoryTreeControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", CategoryTreeControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", CategoryTreeControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", CategoryTreeControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", CategoryTreeControllerTest.DEFAULT_USERNAME).build();
    String requestBody = this.objectMapper.writeValueAsString(this.generateCategoryNodeFilterCategoryCodesRequest());
    this.mockMvc.perform(
        MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON_VALUE)).andExpect(
        MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.categoryTreeService).findCategoryNodeByCatalogCodeAndCategoryCodesAndActive(
        Mockito.anyString(), Mockito.anyList(), Mockito.anyBoolean());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void filterCategoryNodeByCategoryCodesWithCatalogCodeExceptionTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(CategoryTreeControllerPath.BASE_PATH + CategoryTreeControllerPath.FILTER_NODE_CATEGORY_CODES)
            .addParameter("storeId", CategoryTreeControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", CategoryTreeControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", CategoryTreeControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", CategoryTreeControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", CategoryTreeControllerTest.DEFAULT_USERNAME).build();
    CategoryNodeFilterCategoryCodesRequest request = this.generateCategoryNodeFilterCategoryCodesRequest();
    request.setCatalogCode(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON_VALUE))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.categoryTreeService, CategoryTreeControllerTest.NEVER_CALLED)
          .findCategoryNodeByCatalogCodeAndCategoryCodesAndActive(Mockito.anyString(), Mockito.anyList(),
              Mockito.anyBoolean());
    }
  }

  @SuppressWarnings("unchecked")
  @Test
  public void filterCategoryNodeByCategoryCodesWithCategoryCodesIsNullExceptionTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(CategoryTreeControllerPath.BASE_PATH + CategoryTreeControllerPath.FILTER_NODE_CATEGORY_CODES)
            .addParameter("storeId", CategoryTreeControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", CategoryTreeControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", CategoryTreeControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", CategoryTreeControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", CategoryTreeControllerTest.DEFAULT_USERNAME).build();
    CategoryNodeFilterCategoryCodesRequest request = this.generateCategoryNodeFilterCategoryCodesRequest();
    request.setCategoryCodes(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON_VALUE))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.categoryTreeService, CategoryTreeControllerTest.NEVER_CALLED)
          .findCategoryNodeByCatalogCodeAndCategoryCodesAndActive(Mockito.anyString(), Mockito.anyList(),
              Mockito.anyBoolean());
    }
  }

  @SuppressWarnings("unchecked")
  @Test
  public void filterCategoryNodeByCategoryCodesWithCategoryCodesIsEmptyExceptionTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(CategoryTreeControllerPath.BASE_PATH + CategoryTreeControllerPath.FILTER_NODE_CATEGORY_CODES)
            .addParameter("storeId", CategoryTreeControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", CategoryTreeControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", CategoryTreeControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", CategoryTreeControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", CategoryTreeControllerTest.DEFAULT_USERNAME).build();
    CategoryNodeFilterCategoryCodesRequest request = this.generateCategoryNodeFilterCategoryCodesRequest();
    request.getCategoryCodes().clear();
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON_VALUE))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.categoryTreeService, CategoryTreeControllerTest.NEVER_CALLED)
          .findCategoryNodeByCatalogCodeAndCategoryCodesAndActive(Mockito.anyString(), Mockito.anyList(),
              Mockito.anyBoolean());
    }
  }

  @Test
  public void filterCategoryNodeByParentCategoryCodeTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(CategoryTreeControllerPath.BASE_PATH + CategoryTreeControllerPath.FILTER_NODE_PARENT_CATEGORY_CODE)
            .addParameter("storeId", CategoryTreeControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", CategoryTreeControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", CategoryTreeControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", CategoryTreeControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", CategoryTreeControllerTest.DEFAULT_USERNAME).build();
    String requestBody =
        this.objectMapper.writeValueAsString(this.generateCategoryNodeFilterParentCategoryCodeRequest());
    this.mockMvc.perform(
        MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON_VALUE)).andExpect(
        MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.categoryTreeService).findCategoryNodeByCatalogCodeAndParentCategoryCodeAndActive(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void filterCategoryNodeByParentCategoryCodeWithCatalogCodeExceptionTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(CategoryTreeControllerPath.BASE_PATH + CategoryTreeControllerPath.FILTER_NODE_PARENT_CATEGORY_CODE)
            .addParameter("storeId", CategoryTreeControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", CategoryTreeControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", CategoryTreeControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", CategoryTreeControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", CategoryTreeControllerTest.DEFAULT_USERNAME).build();
    CategoryNodeFilterParentCategoryCodeRequest request = this.generateCategoryNodeFilterParentCategoryCodeRequest();
    request.setCatalogCode(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON_VALUE))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.categoryTreeService, CategoryTreeControllerTest.NEVER_CALLED)
          .findCategoryNodeByCatalogCodeAndParentCategoryCodeAndActive(Mockito.anyString(), Mockito.anyString(),
              Mockito.anyBoolean());
    }
  }

  @SuppressWarnings("unchecked")
  @Test
  public void filterByCategoryCodesTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(CategoryTreeControllerPath.BASE_PATH + CategoryTreeControllerPath.FILTER_CATEGORY_CODES)
            .addParameter("storeId", CategoryTreeControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", CategoryTreeControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", CategoryTreeControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", CategoryTreeControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", CategoryTreeControllerTest.DEFAULT_USERNAME).build();
    String requestBody = this.objectMapper.writeValueAsString(this.generateCategoryTreeFilterCategoryCodesRequest());
    this.mockMvc.perform(
        MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON_VALUE)).andExpect(
        MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.categoryTreeService).findByCatalogCodeAndCategoryCodesAndActive(Mockito.anyString(),
        Mockito.anyList(), Mockito.anyBoolean());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void filterByCategoryCodesWithBuildTreeIsFalseTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(CategoryTreeControllerPath.BASE_PATH + CategoryTreeControllerPath.FILTER_CATEGORY_CODES)
            .addParameter("storeId", CategoryTreeControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", CategoryTreeControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", CategoryTreeControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", CategoryTreeControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", CategoryTreeControllerTest.DEFAULT_USERNAME).build();
    CategoryTreeFilterCategoryCodesRequest request = this.generateCategoryTreeFilterCategoryCodesRequest();
    request.setBuildTree(false);
    String requestBody = this.objectMapper.writeValueAsString(request);
    this.mockMvc.perform(
        MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON_VALUE)).andExpect(
        MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.categoryTreeService).findByCatalogCodeAndCategoryCodesAndActive(Mockito.anyString(),
        Mockito.anyList(), Mockito.anyBoolean());
  }

  @SuppressWarnings("unchecked")
  @Test
  public void filterByCategoryCodesWithCatalogCodeExceptionTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(CategoryTreeControllerPath.BASE_PATH + CategoryTreeControllerPath.FILTER_CATEGORY_CODES)
            .addParameter("storeId", CategoryTreeControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", CategoryTreeControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", CategoryTreeControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", CategoryTreeControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", CategoryTreeControllerTest.DEFAULT_USERNAME).build();
    CategoryTreeFilterCategoryCodesRequest request = this.generateCategoryTreeFilterCategoryCodesRequest();
    request.setCatalogCode(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON_VALUE))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.categoryTreeService, CategoryTreeControllerTest.NEVER_CALLED)
          .findByCatalogCodeAndCategoryCodesAndActive(Mockito.anyString(), Mockito.anyList(), Mockito.anyBoolean());
    }
  }

  @SuppressWarnings("unchecked")
  @Test
  public void filterByCategoryCodesWithCategoryCodesIsNullExceptionTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(CategoryTreeControllerPath.BASE_PATH + CategoryTreeControllerPath.FILTER_CATEGORY_CODES)
            .addParameter("storeId", CategoryTreeControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", CategoryTreeControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", CategoryTreeControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", CategoryTreeControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", CategoryTreeControllerTest.DEFAULT_USERNAME).build();
    CategoryTreeFilterCategoryCodesRequest request = this.generateCategoryTreeFilterCategoryCodesRequest();
    request.setCategoryCodes(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON_VALUE))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.categoryTreeService, CategoryTreeControllerTest.NEVER_CALLED)
          .findByCatalogCodeAndCategoryCodesAndActive(Mockito.anyString(), Mockito.anyList(), Mockito.anyBoolean());
    }
  }

  @SuppressWarnings("unchecked")
  @Test
  public void filterByCategoryCodesWithCategoryCodesIsEmptyExceptionTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(CategoryTreeControllerPath.BASE_PATH + CategoryTreeControllerPath.FILTER_CATEGORY_CODES)
            .addParameter("storeId", CategoryTreeControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", CategoryTreeControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", CategoryTreeControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", CategoryTreeControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", CategoryTreeControllerTest.DEFAULT_USERNAME).build();
    CategoryTreeFilterCategoryCodesRequest request = this.generateCategoryTreeFilterCategoryCodesRequest();
    request.getCategoryCodes().clear();
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON_VALUE))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.categoryTreeService, CategoryTreeControllerTest.NEVER_CALLED)
          .findByCatalogCodeAndCategoryCodesAndActive(Mockito.anyString(), Mockito.anyList(), Mockito.anyBoolean());
    }
  }

}
