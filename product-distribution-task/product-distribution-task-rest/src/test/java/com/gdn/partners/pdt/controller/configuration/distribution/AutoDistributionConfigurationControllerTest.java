package com.gdn.partners.pdt.controller.configuration.distribution;

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
import com.gdn.partners.pdt.controller.configuration.distribution.AutoDistributionConfigurationController;
import com.gdn.partners.pdt.controller.configuration.distribution.AutoDistributionConfigurationControllerPath;
import com.gdn.partners.pdt.dto.configuration.distribution.AutoDistributionConfigurationRequest;
import com.gdn.partners.pdt.dto.configuration.distribution.CreateAutoDistributionConfigurationRequest;
import com.gdn.partners.pdt.dto.configuration.distribution.UpdateAutoDistributionConfigurationRequest;
import com.gdn.partners.pdt.entity.configuration.distribution.AutoDistributionConfiguration;
import com.gdn.partners.pdt.service.configuration.distribution.AutoDistributionConfigurationService;

public class AutoDistributionConfigurationControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "API";
  private static final String DEFAULT_CLIENT_ID = "TEST";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_USERNAME = "DEVELOPER";
  private static final String DEFAULT_VENDOR_CODE = "V-00001";
  private static final String DEFAULT_PRIORITY_VALUE = "VALUE";
  private static final VerificationMode NEVER_CALLED = Mockito.times(0);

  @Mock
  private AutoDistributionConfigurationService autoDistributionConfigurationService;

  @InjectMocks
  private AutoDistributionConfigurationController autoDistributionConfigurationController;

  private MockMvc mockMvc;
  private ObjectMapper objectMapper;

  private AutoDistributionConfiguration generateAutoDistributionConfiguration() throws Exception {
    AutoDistributionConfiguration autoDistributionConfiguration = new AutoDistributionConfiguration();
    return autoDistributionConfiguration;
  }

  private List<AutoDistributionConfiguration> generateAutoDistributionConfigurations() throws Exception {
    List<AutoDistributionConfiguration> autoDistributionConfigurations = new ArrayList<AutoDistributionConfiguration>();
    autoDistributionConfigurations.add(this.generateAutoDistributionConfiguration());
    return autoDistributionConfigurations;
  }

  private AutoDistributionConfigurationRequest generateAutoDistributionConfigurationRequest() throws Exception {
    AutoDistributionConfigurationRequest request = new AutoDistributionConfigurationRequest();
    request.setPriorityType("INITIATOR");
    request.setPriorityValue(AutoDistributionConfigurationControllerTest.DEFAULT_PRIORITY_VALUE);
    return request;
  }

  private CreateAutoDistributionConfigurationRequest generateCreateAutoDistributionConfigurationRequest()
      throws Exception {
    CreateAutoDistributionConfigurationRequest request = new CreateAutoDistributionConfigurationRequest();
    request.setVendorCode(AutoDistributionConfigurationControllerTest.DEFAULT_VENDOR_CODE);
    request.setAutoDistributionConfigurations(new ArrayList<AutoDistributionConfigurationRequest>());
    request.getAutoDistributionConfigurations().add(this.generateAutoDistributionConfigurationRequest());
    return request;
  }

  private UpdateAutoDistributionConfigurationRequest generateUpdateAutoDistributionConfigurationRequest()
      throws Exception {
    UpdateAutoDistributionConfigurationRequest request = new UpdateAutoDistributionConfigurationRequest();
    request.setVendorCode(AutoDistributionConfigurationControllerTest.DEFAULT_VENDOR_CODE);
    request.setAutoDistributionConfigurations(new ArrayList<AutoDistributionConfigurationRequest>());
    request.getAutoDistributionConfigurations().add(this.generateAutoDistributionConfigurationRequest());
    return request;
  }

  @SuppressWarnings("unchecked")
  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.openMocks(this);
    this.mockMvc =
        MockMvcBuilders
            .standaloneSetup(this.autoDistributionConfigurationController)
            .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
                new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
                new MappingJackson2HttpMessageConverter()).build();
    this.objectMapper = new ObjectMapper(new JsonFactory());
    List<AutoDistributionConfiguration> autoDistributionConfigurations = this.generateAutoDistributionConfigurations();
    Mockito.when(this.autoDistributionConfigurationService.findByVendorCode(Mockito.anyString())).thenReturn(
        autoDistributionConfigurations);
    Mockito.doNothing().when(this.autoDistributionConfigurationService).create(Mockito.anyString(), Mockito.anyList());
    Mockito.doNothing().when(this.autoDistributionConfigurationService).update(Mockito.anyString(), Mockito.anyList());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.autoDistributionConfigurationService);
  }

  @Test
   void filterByVendorCodeTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                AutoDistributionConfigurationControllerPath.BASE_PATH
                    + AutoDistributionConfigurationControllerPath.FILTER_VENDOR_CODE)
            .addParameter("storeId", AutoDistributionConfigurationControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", AutoDistributionConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", AutoDistributionConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", AutoDistributionConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", AutoDistributionConfigurationControllerTest.DEFAULT_USERNAME)
            .addParameter("vendorCode", AutoDistributionConfigurationControllerTest.DEFAULT_VENDOR_CODE).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.autoDistributionConfigurationService).findByVendorCode(Mockito.anyString());
  }

  @SuppressWarnings("unchecked")
  @Test
   void createTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                AutoDistributionConfigurationControllerPath.BASE_PATH
                    + AutoDistributionConfigurationControllerPath.CREATE)
            .addParameter("storeId", AutoDistributionConfigurationControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", AutoDistributionConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", AutoDistributionConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", AutoDistributionConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", AutoDistributionConfigurationControllerTest.DEFAULT_USERNAME).build();
    String requestBody =
        this.objectMapper.writeValueAsString(this.generateCreateAutoDistributionConfigurationRequest());
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.autoDistributionConfigurationService).create(Mockito.anyString(), Mockito.anyList());
  }

  @SuppressWarnings("unchecked")
  @Test
   void createWithInvalidVendorCodeExceptionTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                AutoDistributionConfigurationControllerPath.BASE_PATH
                    + AutoDistributionConfigurationControllerPath.CREATE)
            .addParameter("storeId", AutoDistributionConfigurationControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", AutoDistributionConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", AutoDistributionConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", AutoDistributionConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", AutoDistributionConfigurationControllerTest.DEFAULT_USERNAME).build();
    CreateAutoDistributionConfigurationRequest request = this.generateCreateAutoDistributionConfigurationRequest();
    request.setVendorCode(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.autoDistributionConfigurationService,
          AutoDistributionConfigurationControllerTest.NEVER_CALLED).create(Mockito.anyString(), Mockito.anyList());
    }
  }

  @SuppressWarnings("unchecked")
  @Test
   void createWithEmptyAutoDistributionConfigurationExceptionTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                AutoDistributionConfigurationControllerPath.BASE_PATH
                    + AutoDistributionConfigurationControllerPath.CREATE)
            .addParameter("storeId", AutoDistributionConfigurationControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", AutoDistributionConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", AutoDistributionConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", AutoDistributionConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", AutoDistributionConfigurationControllerTest.DEFAULT_USERNAME).build();
    CreateAutoDistributionConfigurationRequest request = this.generateCreateAutoDistributionConfigurationRequest();
    request.getAutoDistributionConfigurations().clear();
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.autoDistributionConfigurationService,
          AutoDistributionConfigurationControllerTest.NEVER_CALLED).create(Mockito.anyString(), Mockito.anyList());
    }
  }

  @SuppressWarnings("unchecked")
  @Test
   void createWithAutoDistributionConfigurationIsNullExceptionTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                AutoDistributionConfigurationControllerPath.BASE_PATH
                    + AutoDistributionConfigurationControllerPath.CREATE)
            .addParameter("storeId", AutoDistributionConfigurationControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", AutoDistributionConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", AutoDistributionConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", AutoDistributionConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", AutoDistributionConfigurationControllerTest.DEFAULT_USERNAME).build();
    CreateAutoDistributionConfigurationRequest request = this.generateCreateAutoDistributionConfigurationRequest();
    request.setAutoDistributionConfigurations(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.autoDistributionConfigurationService,
          AutoDistributionConfigurationControllerTest.NEVER_CALLED).create(Mockito.anyString(), Mockito.anyList());
    }
  }

  @SuppressWarnings("unchecked")
  @Test
   void createWithInvalidPriorityTypeExceptionTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                AutoDistributionConfigurationControllerPath.BASE_PATH
                    + AutoDistributionConfigurationControllerPath.CREATE)
            .addParameter("storeId", AutoDistributionConfigurationControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", AutoDistributionConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", AutoDistributionConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", AutoDistributionConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", AutoDistributionConfigurationControllerTest.DEFAULT_USERNAME).build();
    CreateAutoDistributionConfigurationRequest request = this.generateCreateAutoDistributionConfigurationRequest();
    request.getAutoDistributionConfigurations().get(0).setPriorityType(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.autoDistributionConfigurationService,
          AutoDistributionConfigurationControllerTest.NEVER_CALLED).create(Mockito.anyString(), Mockito.anyList());
    }
  }

  @SuppressWarnings("unchecked")
  @Test
   void updateTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                AutoDistributionConfigurationControllerPath.BASE_PATH
                    + AutoDistributionConfigurationControllerPath.UPDATE)
            .addParameter("storeId", AutoDistributionConfigurationControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", AutoDistributionConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", AutoDistributionConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", AutoDistributionConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", AutoDistributionConfigurationControllerTest.DEFAULT_USERNAME).build();
    String requestBody =
        this.objectMapper.writeValueAsString(this.generateUpdateAutoDistributionConfigurationRequest());
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.autoDistributionConfigurationService).update(Mockito.anyString(), Mockito.anyList());
  }

  @SuppressWarnings("unchecked")
  @Test
   void updateWithInvalidVendorCodeExceptionTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                AutoDistributionConfigurationControllerPath.BASE_PATH
                    + AutoDistributionConfigurationControllerPath.UPDATE)
            .addParameter("storeId", AutoDistributionConfigurationControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", AutoDistributionConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", AutoDistributionConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", AutoDistributionConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", AutoDistributionConfigurationControllerTest.DEFAULT_USERNAME).build();
    UpdateAutoDistributionConfigurationRequest request = this.generateUpdateAutoDistributionConfigurationRequest();
    request.setVendorCode(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.autoDistributionConfigurationService,
          AutoDistributionConfigurationControllerTest.NEVER_CALLED).update(Mockito.anyString(), Mockito.anyList());
    }
  }

  @SuppressWarnings("unchecked")
  @Test
   void updateWithEmptyAutoDistributionConfigurationExceptionTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                AutoDistributionConfigurationControllerPath.BASE_PATH
                    + AutoDistributionConfigurationControllerPath.UPDATE)
            .addParameter("storeId", AutoDistributionConfigurationControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", AutoDistributionConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", AutoDistributionConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", AutoDistributionConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", AutoDistributionConfigurationControllerTest.DEFAULT_USERNAME).build();
    UpdateAutoDistributionConfigurationRequest request = this.generateUpdateAutoDistributionConfigurationRequest();
    request.getAutoDistributionConfigurations().clear();
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.autoDistributionConfigurationService,
          AutoDistributionConfigurationControllerTest.NEVER_CALLED).update(Mockito.anyString(), Mockito.anyList());
    }
  }

  @SuppressWarnings("unchecked")
  @Test
   void updateWithAutoDistributionConfigurationIsNullExceptionTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                AutoDistributionConfigurationControllerPath.BASE_PATH
                    + AutoDistributionConfigurationControllerPath.UPDATE)
            .addParameter("storeId", AutoDistributionConfigurationControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", AutoDistributionConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", AutoDistributionConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", AutoDistributionConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", AutoDistributionConfigurationControllerTest.DEFAULT_USERNAME).build();
    UpdateAutoDistributionConfigurationRequest request = this.generateUpdateAutoDistributionConfigurationRequest();
    request.setAutoDistributionConfigurations(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.autoDistributionConfigurationService,
          AutoDistributionConfigurationControllerTest.NEVER_CALLED).update(Mockito.anyString(), Mockito.anyList());
    }
  }

  @SuppressWarnings("unchecked")
  @Test
   void updateWithInvalidPriorityValueExceptionTest() throws Exception {
    URI uri =
        new URIBuilder()
            .setPath(
                AutoDistributionConfigurationControllerPath.BASE_PATH
                    + AutoDistributionConfigurationControllerPath.UPDATE)
            .addParameter("storeId", AutoDistributionConfigurationControllerTest.DEFAULT_STORE_ID)
            .addParameter("channelId", AutoDistributionConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .addParameter("clientId", AutoDistributionConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .addParameter("requestId", AutoDistributionConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .addParameter("username", AutoDistributionConfigurationControllerTest.DEFAULT_USERNAME).build();
    UpdateAutoDistributionConfigurationRequest request = this.generateUpdateAutoDistributionConfigurationRequest();
    request.getAutoDistributionConfigurations().get(0).setPriorityValue(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON)).andExpect(
          MockMvcResultMatchers.status().isOk());
    } catch (Exception e) {
      Mockito.verify(this.autoDistributionConfigurationService,
          AutoDistributionConfigurationControllerTest.NEVER_CALLED).update(Mockito.anyString(), Mockito.anyList());
    }
  }

}
