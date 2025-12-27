package com.gdn.mta.product.controller;

import java.net.URI;
import java.util.Calendar;
import java.util.UUID;
import org.apache.http.client.utils.URIBuilder;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
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
import jakarta.servlet.ServletException;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductBusinessPartnerConfigRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.web.model.ProductBusinessPartnerConfigControllerPath;
import com.gdn.partners.pbp.service.bpconfig.ProductBusinessPartnerConfigService;

/**
 * Created by Vishal on 19/05/18.
 */
public class ProductBusinessPartnerConfigControllerTest {


  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "channel-1";
  private static final String DEFAULT_CLIENT_ID = "client-1";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00001";

  private MockMvc mockMvc;
  private String requestId;
  private ObjectMapper objectMapper;

  @Mock
  private ProductBusinessPartnerConfigService partnerConfigService;

  @InjectMocks
  private ProductBusinessPartnerConfigController partnerConfigController;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(partnerConfigController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).build();
    requestId = UUID.randomUUID().toString();
    objectMapper = new ObjectMapper(new JsonFactory());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(partnerConfigService);
  }

  @Test
  public void notifyMailVisibilityOptionForProductWipTest_success() throws Exception {
    Mockito.when(partnerConfigService
        .notifyMailVisibilityOptionForProductWip(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(Boolean.TRUE);
    URI uri = new URIBuilder().setPath(ProductBusinessPartnerConfigControllerPath.BASE_PATH
        + ProductBusinessPartnerConfigControllerPath.ENABLE_PRODUCT_MAIL_NOTIFY)
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
        .build();
    mockMvc.perform(MockMvcRequestBuilders.get(uri).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", CoreMatchers.equalTo(true)));
    Mockito.verify(partnerConfigService)
        .notifyMailVisibilityOptionForProductWip(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void notifyMailVisibilityOptionForProductWipTest_false() throws Exception {
    Mockito.when(partnerConfigService
        .notifyMailVisibilityOptionForProductWip(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(Boolean.FALSE);
    URI uri = new URIBuilder().setPath(ProductBusinessPartnerConfigControllerPath.BASE_PATH
        + ProductBusinessPartnerConfigControllerPath.ENABLE_PRODUCT_MAIL_NOTIFY)
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
        .build();
    mockMvc.perform(MockMvcRequestBuilders.get(uri).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", CoreMatchers.equalTo(false)));
    Mockito.verify(partnerConfigService)
        .notifyMailVisibilityOptionForProductWip(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void notifyMailVisibilityOptionForProductWipTest_fail() throws Exception {
    Mockito.when(partnerConfigService
        .notifyMailVisibilityOptionForProductWip(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenThrow(new ApplicationRuntimeException());
    URI uri = new URIBuilder().setPath(ProductBusinessPartnerConfigControllerPath.BASE_PATH
        + ProductBusinessPartnerConfigControllerPath.ENABLE_PRODUCT_MAIL_NOTIFY)
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
        .build();
    try {
      mockMvc.perform(MockMvcRequestBuilders.get(uri).contentType(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      if (e.getCause() instanceof ApplicationRuntimeException) {
        Mockito.verify(partnerConfigService)
            .notifyMailVisibilityOptionForProductWip(DEFAULT_STORE_ID,
                DEFAULT_BUSINESS_PARTNER_CODE);
      }
    }
  }

  @Test
  public void saveTest_success() throws Exception {
    ProductBusinessPartnerConfigRequest request =
        new ProductBusinessPartnerConfigRequest(DEFAULT_BUSINESS_PARTNER_CODE,
            Calendar.getInstance().getTime());
    Mockito.doNothing().when(partnerConfigService)
        .save(DEFAULT_STORE_ID, DEFAULT_USERNAME, request);
    URI uri = new URIBuilder().setPath(ProductBusinessPartnerConfigControllerPath.BASE_PATH
        + ProductBusinessPartnerConfigControllerPath.SAVE).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
        .build();
    mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .content(this.objectMapper.writeValueAsString(request))
        .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(partnerConfigService)
        .save(Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(DEFAULT_USERNAME),
            Mockito.any(ProductBusinessPartnerConfigRequest.class));
  }

  @Test
  public void saveTest_nullBpCode_failed() throws Exception {
    ProductBusinessPartnerConfigRequest request =
        new ProductBusinessPartnerConfigRequest(null, Calendar.getInstance().getTime());
    URI uri = new URIBuilder().setPath(ProductBusinessPartnerConfigControllerPath.BASE_PATH
        + ProductBusinessPartnerConfigControllerPath.SAVE).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
        .build();
    try {
      Assertions.assertThrows(ServletException.class, () -> {
        mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
                .content(this.objectMapper.writeValueAsString(request))
                .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk())
            .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
      });
    } catch (Exception e) {
      if (e.getCause() instanceof ApplicationRuntimeException)
        throw new ApplicationRuntimeException();
    }
  }

  @Test
  public void saveTest_nullDate_failed() throws Exception {
    ProductBusinessPartnerConfigRequest request =
        new ProductBusinessPartnerConfigRequest(DEFAULT_BUSINESS_PARTNER_CODE, null);
    URI uri = new URIBuilder().setPath(ProductBusinessPartnerConfigControllerPath.BASE_PATH
        + ProductBusinessPartnerConfigControllerPath.SAVE).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
        .build();
    try {
      Assertions.assertThrows(ServletException.class, () -> {
        mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
                .content(this.objectMapper.writeValueAsString(request))
                .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk())
            .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
      });
    } catch (Exception e) {
      if (e.getCause() instanceof ApplicationRuntimeException)
        throw new ApplicationRuntimeException();
    }
  }

  @Test
  public void saveTest_throwsException_failed() throws Exception {
    ProductBusinessPartnerConfigRequest request =
        new ProductBusinessPartnerConfigRequest(DEFAULT_BUSINESS_PARTNER_CODE,
            Calendar.getInstance().getTime());
    Mockito.doThrow(new ApplicationRuntimeException()).when(partnerConfigService)
        .save(Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(DEFAULT_USERNAME),
            Mockito.any(ProductBusinessPartnerConfigRequest.class));
    URI uri = new URIBuilder().setPath(ProductBusinessPartnerConfigControllerPath.BASE_PATH
        + ProductBusinessPartnerConfigControllerPath.SAVE).addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).addParameter("clientId", DEFAULT_CLIENT_ID)
        .build();
    try {
      mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
          .content(this.objectMapper.writeValueAsString(request))
          .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    } catch (Exception e) {
      if (e.getCause() instanceof ApplicationRuntimeException) {
        Mockito.verify(partnerConfigService)
            .save(Mockito.eq(DEFAULT_STORE_ID), Mockito.eq(DEFAULT_USERNAME),
                Mockito.any(ProductBusinessPartnerConfigRequest.class));
      }
    }
  }

}
