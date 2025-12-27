package com.gdn.mta.product.controller.generator;

import java.net.URI;
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
import com.gda.mta.product.dto.generator.GenerateShippingWeightRequest;
import com.gdn.mta.product.service.generator.GeneratorService;
import com.gdn.mta.product.web.model.GeneratorControllerPath;

public class GeneratorControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "API";
  private static final String DEFAULT_CLIENT_ID = "TEST";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_USERNAME = "DEVELOPER";
  private static final String DEFAULT_CATEGORY_CODE = "CAT-0000001";
  private static final VerificationMode NEVER_CALLED = Mockito.times(0);

  @Mock
  private GeneratorService generatorService;

  @InjectMocks
  private GeneratorController generatorController;

  private MockMvc mockMvc;
  private ObjectMapper objectMapper;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc =
        MockMvcBuilders
            .standaloneSetup(this.generatorController)
            .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
                new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
                new MappingJackson2HttpMessageConverter()).build();
    this.objectMapper = new ObjectMapper(new JsonFactory());
    Mockito.when(
        this.generatorService.generateShippingWeight(Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(),
            Mockito.anyDouble(), Mockito.anyString())).thenReturn(1D);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.generatorService);
  }

  public GenerateShippingWeightRequest generateGenerateShippingWeightRequest() throws Exception {
    GenerateShippingWeightRequest request = new GenerateShippingWeightRequest();
    request.setLength(1D);
    request.setWidth(1D);
    request.setHeight(1D);
    request.setWeight(1D);
    request.setCategoryCode(GeneratorControllerTest.DEFAULT_CATEGORY_CODE);
    return request;
  }

  @Test
  public void generateShippingWeightTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(GeneratorControllerPath.BASE_PATH + GeneratorControllerPath.GENERATE_SHIPPING_WEIGHT)
            .addParameter("storeId", GeneratorControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", GeneratorControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", GeneratorControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", GeneratorControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", GeneratorControllerTest.DEFAULT_USERNAME).build();
    String requestBody = this.objectMapper.writeValueAsString(this.generateGenerateShippingWeightRequest());
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.generatorService).generateShippingWeight(Mockito.anyDouble(), Mockito.anyDouble(),
        Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyString());
  }

  @Test
  public void generateShippingWeightWithLengthExceptionTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(GeneratorControllerPath.BASE_PATH + GeneratorControllerPath.GENERATE_SHIPPING_WEIGHT)
            .addParameter("storeId", GeneratorControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", GeneratorControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", GeneratorControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", GeneratorControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", GeneratorControllerTest.DEFAULT_USERNAME).build();
    GenerateShippingWeightRequest request = this.generateGenerateShippingWeightRequest();
    request.setLength(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.generatorService, GeneratorControllerTest.NEVER_CALLED).generateShippingWeight(
          Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyString());
    }
  }

  @Test
  public void generateShippingWeightWithWidthExceptionTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(GeneratorControllerPath.BASE_PATH + GeneratorControllerPath.GENERATE_SHIPPING_WEIGHT)
            .addParameter("storeId", GeneratorControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", GeneratorControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", GeneratorControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", GeneratorControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", GeneratorControllerTest.DEFAULT_USERNAME).build();
    GenerateShippingWeightRequest request = this.generateGenerateShippingWeightRequest();
    request.setWidth(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.generatorService, GeneratorControllerTest.NEVER_CALLED).generateShippingWeight(
          Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyString());
    }
  }

  @Test
  public void generateShippingWeightWithHeightExceptionTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(GeneratorControllerPath.BASE_PATH + GeneratorControllerPath.GENERATE_SHIPPING_WEIGHT)
            .addParameter("storeId", GeneratorControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", GeneratorControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", GeneratorControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", GeneratorControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", GeneratorControllerTest.DEFAULT_USERNAME).build();
    GenerateShippingWeightRequest request = this.generateGenerateShippingWeightRequest();
    request.setHeight(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.generatorService, GeneratorControllerTest.NEVER_CALLED).generateShippingWeight(
          Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyString());
    }
  }

  @Test
  public void generateShippingWeightWithWeightExceptionTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(GeneratorControllerPath.BASE_PATH + GeneratorControllerPath.GENERATE_SHIPPING_WEIGHT)
            .addParameter("storeId", GeneratorControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", GeneratorControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", GeneratorControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", GeneratorControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", GeneratorControllerTest.DEFAULT_USERNAME).build();
    GenerateShippingWeightRequest request = this.generateGenerateShippingWeightRequest();
    request.setWeight(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.generatorService, GeneratorControllerTest.NEVER_CALLED).generateShippingWeight(
          Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyString());
    }
  }

  @Test
  public void generateShippingWeightWithCategoryCodeExceptionTest() throws Exception {
    URI uri =
        new URIBuilder().setPath(GeneratorControllerPath.BASE_PATH + GeneratorControllerPath.GENERATE_SHIPPING_WEIGHT)
            .addParameter("storeId", GeneratorControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", GeneratorControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", GeneratorControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", GeneratorControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", GeneratorControllerTest.DEFAULT_USERNAME).build();
    GenerateShippingWeightRequest request = this.generateGenerateShippingWeightRequest();
    request.setCategoryCode(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.generatorService, GeneratorControllerTest.NEVER_CALLED).generateShippingWeight(
          Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyDouble(), Mockito.anyString());
    }
  }

}
