package com.gdn.partners.pbp.controller.workflow.product;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.net.URI;
import java.util.ArrayList;
import java.util.UUID;

import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.product.service.ProductWorkflowServiceWrapper;
import com.gdn.partners.pbp.dto.productlevel3.UpdateProductLevel3AttributeWipRequest;
import com.gdn.partners.pbp.dto.productlevel3.UpdateProductLevel3ItemWipRequest;
import com.gdn.partners.pbp.dto.productlevel3.UpdateProductLevel3WipRequest;
import com.gdn.partners.pbp.dto.workflow.product.ProductResubmitRequest;
import com.gdn.partners.pbp.dto.workflow.product.ProductReturnForCorrectionRequest;
import com.gdn.partners.pbp.entity.workflow.product.ProductWorkflowStatus;
import com.gdn.partners.pbp.web.model.ProductWorkflowControllerPath;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;

public class ProductWorkflowControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "API";
  private static final String DEFAULT_CLIENT_ID = "TEST";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_USERNAME = "DEVELOPER";
  private static final String DEFAULT_PRODUCT_CODE = "MTA-0000001";
  private static final String DEFAULT_NOTES = "test notes";

  @Mock
  private ProductWfService productWorkflowService;

  @Mock
  private ProductWorkflowServiceWrapper productWorkflowServiceWrapper;

  @InjectMocks
  private ProductWorkflowController productWorkflowController;

  @Captor
  private ArgumentCaptor<ProductResubmitRequest> productRequestArgumentCaptor;

  private MockMvc mockMvc;

  private ObjectMapper objectMapper;

  private ProductWorkflowStatus generateProductWorkflowStatus() throws Exception {
    ProductWorkflowStatus productWorkflowStatus = new ProductWorkflowStatus();
    productWorkflowStatus.getStates().add("IN_VENDOR");
    productWorkflowStatus.getStatus().put("DRAFT", true);
    return productWorkflowStatus;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc =
        MockMvcBuilders
            .standaloneSetup(this.productWorkflowController)
            .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
                new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
                new MappingJackson2HttpMessageConverter()).build();
    ProductWorkflowStatus productWorkflowStatus = this.generateProductWorkflowStatus();
    this.objectMapper = new ObjectMapper();
    Mockito.when(this.productWorkflowService.status(Mockito.anyString())).thenReturn(productWorkflowStatus);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productWorkflowService, productWorkflowServiceWrapper);
  }

  @Test
  public void statusTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(ProductWorkflowControllerPath.BASE_PATH + ProductWorkflowControllerPath.STATUS)
            .addParameter("storeId", ProductWorkflowControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductWorkflowControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", ProductWorkflowControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", ProductWorkflowControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductWorkflowControllerTest.DEFAULT_USERNAME)
            .addParameter("productCode", ProductWorkflowControllerTest.DEFAULT_PRODUCT_CODE).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.productWorkflowService).status(Mockito.anyString());
  }

  @Test
  public void statusWithNullProductWorkflowStatusTest() throws Exception {
    Mockito.when(this.productWorkflowService.status(Mockito.anyString())).thenReturn(null);
    URI uri =
        new URIBuilder().setPath(ProductWorkflowControllerPath.BASE_PATH + ProductWorkflowControllerPath.STATUS)
            .addParameter("storeId", ProductWorkflowControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", ProductWorkflowControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", ProductWorkflowControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", ProductWorkflowControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", ProductWorkflowControllerTest.DEFAULT_USERNAME)
            .addParameter("productCode", ProductWorkflowControllerTest.DEFAULT_PRODUCT_CODE).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.productWorkflowService).status(Mockito.anyString());
  }

  private ProductResubmitRequest generateProductResubmitRequest() throws Exception {
    ProductResubmitRequest productResubmitRequest = new ProductResubmitRequest();
    ProductRequest productRequest = new ProductRequest();
    productRequest.setProductCode(DEFAULT_PRODUCT_CODE);
    productRequest.setProductCategories(new ArrayList<ProductCategoryRequest>());
    productRequest.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    productRequest.setProductItems(new ArrayList<ProductItemRequest>());
    productResubmitRequest.setProductRequest(productRequest);
    productResubmitRequest.getProductRequest().setProductCode(ProductWorkflowControllerTest.DEFAULT_PRODUCT_CODE);
    UpdateProductLevel3WipRequest updateProductLevel3WipRequest = new UpdateProductLevel3WipRequest();
    updateProductLevel3WipRequest.setItems(new ArrayList<>());
    updateProductLevel3WipRequest.setAttributes(new ArrayList<>());
    UpdateProductLevel3ItemWipRequest itemWipRequest = new UpdateProductLevel3ItemWipRequest();
    updateProductLevel3WipRequest.getItems().add(itemWipRequest);
    UpdateProductLevel3AttributeWipRequest attributeWipRequest = new UpdateProductLevel3AttributeWipRequest();
    updateProductLevel3WipRequest.getAttributes().add(attributeWipRequest);
    productResubmitRequest.setProductLevel3WipRequest(updateProductLevel3WipRequest);
    return productResubmitRequest;
  }

  @Test
  public void resubmitTest() throws Exception {
    ProductResubmitRequest request = this.generateProductResubmitRequest();
    URI uri = new URIBuilder().setPath(ProductWorkflowControllerPath.BASE_PATH + ProductWorkflowControllerPath.RESUBMIT)
        .addParameter("storeId", ProductWorkflowControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductWorkflowControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductWorkflowControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductWorkflowControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductWorkflowControllerTest.DEFAULT_USERNAME).build();
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).contentType(MediaType.APPLICATION_JSON)
        .content(this.objectMapper.writeValueAsString(request))).andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.productWorkflowServiceWrapper)
        .resubmit(eq(DEFAULT_STORE_ID), productRequestArgumentCaptor.capture(), Mockito.any());
  }

  @Test
  public void resubmitExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.productWorkflowServiceWrapper)
        .resubmit(eq(DEFAULT_STORE_ID), Mockito.any(ProductResubmitRequest.class), Mockito.any());
    ProductResubmitRequest request = this.generateProductResubmitRequest();
    URI uri = new URIBuilder().setPath(ProductWorkflowControllerPath.BASE_PATH + ProductWorkflowControllerPath.RESUBMIT)
        .addParameter("storeId", ProductWorkflowControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductWorkflowControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductWorkflowControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductWorkflowControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductWorkflowControllerTest.DEFAULT_USERNAME).build();
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).contentType(MediaType.APPLICATION_JSON)
        .content(this.objectMapper.writeValueAsString(request))).andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.productWorkflowServiceWrapper)
        .resubmit(eq(DEFAULT_STORE_ID), productRequestArgumentCaptor.capture(), Mockito.any());
  }

  @Test
  public void returnToCorrectionTest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductWorkflowControllerPath.BASE_PATH
            + ProductWorkflowControllerPath.RETURN_FOR_CORRECTION)
        .addParameter("storeId", ProductWorkflowControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductWorkflowControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductWorkflowControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductWorkflowControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductWorkflowControllerTest.DEFAULT_USERNAME).build();
    ProductReturnForCorrectionRequest request = new ProductReturnForCorrectionRequest();
    request.setProductCode(DEFAULT_PRODUCT_CODE);
    request.setNotes(DEFAULT_NOTES);
    MvcResult mvcResult = this.mockMvc
        .perform(post(uri).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(this.objectMapper.writeValueAsString(request)))
        .andExpect(status().isOk()).andReturn();
    assertTrue(this.objectMapper
        .readValue(mvcResult.getResponse().getContentAsString(), GdnBaseRestResponse.class)
        .isSuccess());
    verify(this.productWorkflowServiceWrapper).returnForCorrection(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE, DEFAULT_NOTES);
  }

  @Test
  public void returnToCorrectionExceptionTest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductWorkflowControllerPath.BASE_PATH
            + ProductWorkflowControllerPath.RETURN_FOR_CORRECTION)
        .addParameter("storeId", ProductWorkflowControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductWorkflowControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductWorkflowControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductWorkflowControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductWorkflowControllerTest.DEFAULT_USERNAME).build();
    ProductReturnForCorrectionRequest request = new ProductReturnForCorrectionRequest();
    request.setProductCode(DEFAULT_PRODUCT_CODE);
    request.setNotes(DEFAULT_NOTES);
    Mockito.doThrow(Exception.class).when(this.productWorkflowServiceWrapper)
        .returnForCorrection(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE, DEFAULT_NOTES);
    MvcResult mvcResult = this.mockMvc
        .perform(post(uri).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(this.objectMapper.writeValueAsString(request)))
        .andExpect(status().isOk()).andReturn();
    verify(this.productWorkflowServiceWrapper).returnForCorrection(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE, DEFAULT_NOTES);
  }
}
