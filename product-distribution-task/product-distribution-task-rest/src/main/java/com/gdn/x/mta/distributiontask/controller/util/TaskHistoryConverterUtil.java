package com.gdn.x.mta.distributiontask.controller.util;

import com.gdn.x.mta.distributiontask.model.TaskHistory;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.rest.model.response.TaskHistoryResponse;
import com.google.common.collect.ImmutableMap;
import org.springframework.beans.BeanUtils;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class TaskHistoryConverterUtil {

  private static final String END_LINE_CHARACTER = "\\n";
  private static final String ESCAPED_DOUBLE_QUOTES_AND_SLASH  = "\\\"";
  private static final String ESCAPED_DOUBLE_QUOTES = "\"";

  private static final Map<WorkflowState, String> WORKFLOW_STATE_STRING_MAP =
      ImmutableMap.<com.gdn.x.mta.distributiontask.model.type.WorkflowState, String>builder()
          .put(WorkflowState.UNASSIGNED, "Unassigned").put(WorkflowState.IN_REVIEW, "In Review")
          .put(WorkflowState.REJECTED, "Rejected").put(WorkflowState.QC_REJECTED, "QC Rejected")
          .put(WorkflowState.PASSED, "Passed").put(WorkflowState.EXCEEDED_SLA, "Exceeded SLA")
          .build();

  public static List<TaskHistoryResponse> convertTaskHistoriesToTaskHistoryResponses(
      List<TaskHistory> taskHistories, boolean escapeString) {
    List<TaskHistoryResponse> taskHistoryReponses = new ArrayList<TaskHistoryResponse>();
    if (!CollectionUtils.isEmpty(taskHistories)) {
      for (TaskHistory taskHistory : taskHistories) {
        TaskHistoryResponse taskHistoryResponse = new TaskHistoryResponse();
        BeanUtils.copyProperties(taskHistory, taskHistoryResponse, "vendor", "state");
        taskHistoryResponse.setVendorCode(taskHistory.getVendor().getVendorCode());
        taskHistoryResponse.setVendorName(taskHistory.getVendor().getName());
        taskHistoryResponse.setState(WORKFLOW_STATE_STRING_MAP.get(taskHistory.getState()));
        if (escapeString) {
          taskHistoryResponse
              .setReason(taskHistoryResponse.getReason().replace(END_LINE_CHARACTER, ""));
          taskHistoryResponse.setReason(taskHistoryResponse.getReason()
              .replace(ESCAPED_DOUBLE_QUOTES_AND_SLASH, ESCAPED_DOUBLE_QUOTES));
        }
        taskHistoryReponses.add(taskHistoryResponse);
      }
    }
    return taskHistoryReponses;
  }
}
