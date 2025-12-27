package com.gdn.partners.pbp.controller.productlevel1;

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
import com.gdn.partners.pbp.dto.productlevel1.ProductLevel1WipDeleteRequest;
import com.gdn.partners.pbp.web.model.ProductLevel1WipControllerPath;
import com.gdn.partners.pbp.workflow.product.ProductWfService;

public class ProductLevel1WipControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "API";
  private static final String DEFAULT_CLIENT_ID = "TEST";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_USERNAME = "DEVELOPER";
  private static final String PRODUCT_CODE = "productCode";
  private static final String NOTES = "notes";
  
  @InjectMocks
  private ProductLevel1WipController productLevel1WipController;
  
  @Mock
  private ProductWfService productWfService;

  private MockMvc mockMvc;
  private ObjectMapper objectMapper;

  @BeforeEach
  public void setup() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = MockMvcBuilders.standaloneSetup(this.productLevel1WipController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter())
        .build();
    this.objectMapper = new ObjectMapper(new JsonFactory());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productWfService);
  }

  @Test
  public void deleteByProductCodes_emptyProductCodes_Test() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel1WipControllerPath.BASE_PATH
            + ProductLevel1WipControllerPath.DELETE_BY_PRODUCT_CODES)
        .addParameter("storeId", ProductLevel1WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel1WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel1WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel1WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel1WipControllerTest.DEFAULT_USERNAME).build();
    final ProductLevel1WipDeleteRequest request = this.generateProductLevel1WipDeleteRequest();
    request.setProductCodes(new ArrayList<>());
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc
      .perform(MockMvcRequestBuilders.post(uri).content(requestBody)
          .contentType(MediaType.APPLICATION_JSON));
    } catch (Exception e) {
    }
  }
  
  @Test
  public void deleteByProductCodes_emptyNotes_Test() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel1WipControllerPath.BASE_PATH
            + ProductLevel1WipControllerPath.DELETE_BY_PRODUCT_CODES)
        .addParameter("storeId", ProductLevel1WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel1WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel1WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel1WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel1WipControllerTest.DEFAULT_USERNAME).build();
    final ProductLevel1WipDeleteRequest request = this.generateProductLevel1WipDeleteRequest();
    request.setNotes(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc
      .perform(MockMvcRequestBuilders.post(uri).content(requestBody)
          .contentType(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
    }
  }

  @Test
  public void deleteByProductCodesTest() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel1WipControllerPath.BASE_PATH
            + ProductLevel1WipControllerPath.DELETE_BY_PRODUCT_CODES)
        .addParameter("storeId", ProductLevel1WipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel1WipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel1WipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel1WipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel1WipControllerTest.DEFAULT_USERNAME).build();
    final ProductLevel1WipDeleteRequest request = this.generateProductLevel1WipDeleteRequest();
    String requestBody = this.objectMapper.writeValueAsString(request);
    this.mockMvc
        .perform(MockMvcRequestBuilders.post(uri).content(requestBody)
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.productWfService).delete(request.getProductCodes(),
        request.getNotes());
  }
  
  private ProductLevel1WipDeleteRequest generateProductLevel1WipDeleteRequest() throws Exception {
    ProductLevel1WipDeleteRequest request = new ProductLevel1WipDeleteRequest();
    List<String> productCodes = new ArrayList<>();
    productCodes.add(PRODUCT_CODE);
    request.setProductCodes(productCodes);
    request.setNotes(NOTES);
    return request;
  }
}
