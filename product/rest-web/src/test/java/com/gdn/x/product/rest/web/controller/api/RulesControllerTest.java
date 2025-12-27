package com.gdn.x.product.rest.web.controller.api;

import java.net.URI;
import java.util.UUID;

import org.apache.http.client.utils.URIBuilder;
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

import com.gdn.x.product.rest.web.model.RulesControllerApiPath;
import com.gdn.x.product.rest.web.model.response.ProductScoreRuleResponse;
import com.gdn.x.product.service.api.CategoryService;

public class RulesControllerTest {
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "channel-1";
  private static final String DEFAULT_CLIENT_ID = "client-1";
  private static final String DEFAULT_USERNAME = "com.gdn.mta.developer";
  private static final String CATEGORY_CODE = "CATEGORY_CODE";

  private MockMvc mockMvc;

  @InjectMocks
  private RulesController categoryController;

  @Mock
  private CategoryService categoryService;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(this.categoryController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).build();
  }

  public MockMvc getMockMvc() {
    return this.mockMvc;
  }

  @Test
  public void getProductScoreRuleTest() throws Exception {
    Mockito.when(this.categoryService.getProductScoreRuleForCategory(CATEGORY_CODE)).thenReturn(new ProductScoreRuleResponse());
    URI uri = new URIBuilder().setPath(RulesControllerApiPath.BASE_PATH + RulesControllerApiPath.PRODUCT_SCORE_RULE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("categoryCode", CATEGORY_CODE).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.categoryService).getProductScoreRuleForCategory(CATEGORY_CODE);
  }
}
