package com.gdn.partners.pcu.internal.service.impl;

import static org.mockito.ArgumentMatchers.eq;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.response.AssigneeResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class AssigneeServiceImplTest {

  @InjectMocks
  private AssigneeServiceImpl assigneeService;

  @Mock
  private PBPFeign pbpFeign;

  @Captor
  private ArgumentCaptor<SummaryFilterRequest> filterRequestArgumentCaptor;
  private GdnRestListResponse<AssigneeResponse> assigneeResponses;
  private List<AssigneeResponse> assigneeResponseList;
  private static final String ASSIGNEE = "ASSIGNEE";

  @BeforeEach
  public void setUp() throws Exception {
    AssigneeResponse assigneeResponse = new AssigneeResponse();
    assigneeResponse.setAssignee(ASSIGNEE);
    assigneeResponseList = Arrays.asList(assigneeResponse);
    assigneeResponses = new GdnRestListResponse<>(assigneeResponseList, new PageMetaData(), Constants.REQUEST_ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pbpFeign);
  }

  @Test
  public void getAssigneesByFilterRequestAndActivatedAndViewableFlagTest() {
    ReviewProductsFilterRequest request =
        ReviewProductsFilterRequest.builder().statusFilter(Constants.DEFAULT_FILTER_STATE).timeFilter(Constants.DEFAULT_FILTER_STATE).build();
    Mockito.when(this.pbpFeign
        .getAssigneesByFilterRequestAndActivatedAndViewableFlag(Mockito.any(SummaryFilterRequest.class),
            eq(Boolean.FALSE), eq(Boolean.FALSE))).thenReturn(assigneeResponses);
    List<String> assignees = this.assigneeService
        .getAssigneesByFilterRequestAndActivatedAndViewableFlag(request, Boolean.FALSE, Boolean.FALSE);
    Mockito.verify(this.pbpFeign)
        .getAssigneesByFilterRequestAndActivatedAndViewableFlag(filterRequestArgumentCaptor.capture(), eq(Boolean.FALSE),
            eq(Boolean.FALSE));
    Assertions.assertNotNull(assignees);
    Assertions.assertEquals(assignees.get(0), ASSIGNEE);
  }
}
