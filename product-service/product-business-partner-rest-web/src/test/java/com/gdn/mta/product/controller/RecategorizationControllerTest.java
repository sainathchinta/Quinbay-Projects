package com.gdn.mta.product.controller;

import static org.junit.jupiter.api.Assertions.assertEquals;
import java.net.URI;
import org.apache.http.client.utils.URIBuilder;
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
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.RecategorizationRequest;
import com.gdn.mta.domain.event.config.RecategorizationStatus;
import com.gdn.mta.product.service.RecategorizationService;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.mta.product.web.model.RecategorizationControllerPath;

/**
 * Created by hardikbohra on 07/06/18.
 */
public class RecategorizationControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "channel-1";
  private static final String DEFAULT_CLIENT_ID = "client-1";
  private static final String DEFAULT_USERNAME = "com.gdn.mta.developer";
  private static final String DEFAULT_REQUEST_ID = "requestId";

  @InjectMocks
  private RecategorizationController controller;

  @Mock
  private RecategorizationService recategorizationService;

  private MockMvc mockMvc;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders
        .standaloneSetup(controller)
        .setMessageConverters(new ByteArrayHttpMessageConverter(),
            new StringHttpMessageConverter(), new ResourceHttpMessageConverter(),
            new FormHttpMessageConverter(), new MappingJackson2HttpMessageConverter()).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(recategorizationService);
  }

  @Test
  public void createRecategorizationTest_whenRequiredParametersMissing() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    RecategorizationRequest request = new RecategorizationRequest();
    URI uri =
        new URIBuilder()
            .setPath(
                RecategorizationControllerPath.BASE_PATH + RecategorizationControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();
    MvcResult result = null;
    try {
      result = mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType
          (MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(request))).andExpect
          (MockMvcResultMatchers.status().isOk()).andReturn();
    } catch (Exception ex) {
      Assertions.assertNull(result);
    }
  }

  @Test
  public void createRecategorizationTest() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper(new JsonFactory());
    RecategorizationRequest request = new RecategorizationRequest("recateName", RecategorizationStatus.IN_PROGRESS
        .toString(), "excelFile");
    request.setId("id");
    GdnRestSimpleResponse<String> response = new GdnRestSimpleResponse(null, null, true, DEFAULT_REQUEST_ID, null);
    URI uri =
        new URIBuilder()
            .setPath(
                RecategorizationControllerPath.BASE_PATH + RecategorizationControllerPath.CREATE)
            .addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("requestId", DEFAULT_REQUEST_ID)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("clientId", DEFAULT_CLIENT_ID).build();

    MvcResult result = mockMvc.perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON).contentType
        (MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(request))).andExpect
        (MockMvcResultMatchers.status().isOk()).andReturn();
    assertEquals(objectMapper.writeValueAsString(response), result.getResponse().getContentAsString());
    Mockito.verify(recategorizationService).save(Mockito.any());
  }
}
