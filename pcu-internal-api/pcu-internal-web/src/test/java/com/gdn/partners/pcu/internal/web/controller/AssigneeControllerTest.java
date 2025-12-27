package com.gdn.partners.pcu.internal.web.controller;

import java.util.Arrays;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.AssigneeApiPath;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.service.AssigneeService;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@AutoConfigureMockMvc
@ExtendWith(MockitoExtension.class)
public class AssigneeControllerTest extends TestHelper {

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private AssigneeService assigneeService;

  @Autowired
  public void setMockMvc(MockMvc mockMvc) {
    this.mockMvc = mockMvc;
    super.setMockMvc(mockMvc);
  }

  @InjectMocks
  private AssigneeController assigneeController;

  @Captor
  private ArgumentCaptor<ReviewProductsFilterRequest> requestArgumentCaptor;

  private static final String ASSIGNEE = "ASSIGNEE";

  @BeforeEach
  public void setUp() throws Exception {
    mockMvc = MockMvcBuilders.standaloneSetup(assigneeController).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(assigneeService, clientParameterHelper);
  }

  @Test
  public void getAssigneesByTimeAndStatusFilter() throws Exception {
    ReviewProductsFilterRequest reviewProductsFilterRequest =
        ReviewProductsFilterRequest.builder().statusFilter(Constants.DEFAULT_FILTER_STATE).timeFilter(Constants.DEFAULT_FILTER_STATE)
            .searchKeyword(StringUtils.EMPTY).build();
    when(this.assigneeService
        .getAssigneesByFilterRequestAndActivatedAndViewableFlag(Mockito.any(ReviewProductsFilterRequest.class),
            eq(Boolean.FALSE), eq(Boolean.FALSE))).thenReturn(Arrays.asList(ASSIGNEE));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder = post(AssigneeApiPath.BASE_PATH + AssigneeApiPath.ASSIGNEE_FILTER)
        .param("activated", String.valueOf(Boolean.FALSE)).param("viewable", String.valueOf(Boolean.FALSE))
        .content(toJson(reviewProductsFilterRequest)).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(this.assigneeService).getAssigneesByFilterRequestAndActivatedAndViewableFlag(requestArgumentCaptor.capture(), eq(Boolean.FALSE),
        eq(Boolean.FALSE));
  }
}