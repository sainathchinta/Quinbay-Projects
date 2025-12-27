package com.gdn.partners.pcu.internal.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.LookupControllerPath;
import com.gdn.partners.pcu.internal.service.LookupService;
import com.gdn.partners.pcu.internal.web.TestApplication;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;

@AutoConfigureMockMvc
public class LookupControllerTest extends TestHelper {

  @Mock
  private LookupService lookupService;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @InjectMocks
  private LookupController lookupController;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(this.lookupController).build();
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(lookupService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
  }


  @Test
  public void getDangerousGoodsLevelTest() throws Exception {
    Mockito.when(this.lookupService.getDangerousGoodsLevel()).thenReturn(new ArrayList<>());
    Mockito.when(clientParameterHelper.getRequestId())
        .thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(LookupControllerPath.BASE_PATH + LookupControllerPath.DANGEROUS_GOODS_LEVEL);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.lookupService).getDangerousGoodsLevel();
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }
}