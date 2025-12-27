package com.gdn.x.mta.distributiontask.controller;

import java.util.ArrayList;
import java.util.List;

import com.gdn.x.mta.distributiontask.model.Constants;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.mta.distributiontask.controller.util.TaskHistoryConverterUtil;
import com.gdn.x.mta.distributiontask.model.TaskHistory;
import com.gdn.x.mta.distributiontask.rest.model.response.TaskHistoryResponse;
import com.gdn.x.mta.distributiontask.service.api.TaskHistoryService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@Controller
@RequestMapping
@Tag(name = "Task History Controller", description = "Task History Controller")
@Slf4j
public class TaskHistoryController {
  public static final String BASE_PATH = "/task-history";
  public static final String TASK_HISTORY_SUMMARY_BY_PRODUCT_CODE =
      BASE_PATH + "/task-history-summary-by-product-code";
  public static final String TASK_HISTORY_SUMMARY_BY_TASK_CODE =
      BASE_PATH + "/task-history-summary-by-task-code";

  @Autowired
  private TaskHistoryService taskHistoryService;

  @RequestMapping(value = TASK_HISTORY_SUMMARY_BY_PRODUCT_CODE, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "task history summary by product code", description = "task history summary by product code")
  public GdnRestListResponse<TaskHistoryResponse> taskHistorySummaryByProductCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String productCode,
      @RequestParam(defaultValue = "false") boolean escapeString) throws Exception {
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    boolean isSuccess = false;
    String errorMessage = StringUtils.EMPTY;
    List<TaskHistoryResponse> response = new ArrayList<TaskHistoryResponse>();
    long total = 0;
    try {
      Page<TaskHistory> taskHistories = this.taskHistoryService.getHistoryFromProductCode(storeId,
          productCode, PageRequest.of(page, size, Sort.by(Direction.DESC, "createdDate")));
      if (taskHistories != null) {
        total = taskHistories.getTotalElements();
        response = TaskHistoryConverterUtil
            .convertTaskHistoriesToTaskHistoryResponses(taskHistories.getContent(), escapeString);
      }
      isSuccess = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error(
          "Error retrieving product history for given product code {}. MandatoryRequestParam: {} error {}",
          productCode, mandatoryRequestParam, e.getMessage(), e);
    }
    return new GdnRestListResponse<TaskHistoryResponse>(errorMessage, null, isSuccess, response,
        new PageMetaData(size, page, total), requestId);
  }

  @RequestMapping(value = TASK_HISTORY_SUMMARY_BY_TASK_CODE, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "task history summary by task code", description = "task history summary by task code")
  @Deprecated(since = Constants.DEPRECATED_VERSION, forRemoval = true)
  public GdnRestListResponse<TaskHistoryResponse> taskHistorySummaryByTaskCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String taskCode)
      throws Exception {
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    boolean isSuccess = false;
    String errorMessage = StringUtils.EMPTY;
    List<TaskHistoryResponse> response = new ArrayList<TaskHistoryResponse>();
    try {
      Page<TaskHistory> taskHistories = this.taskHistoryService.getHistoryFromTaskCode(storeId,
          taskCode, PageRequest.of(page, size, Sort.by(Direction.DESC, "createdDate")));
      if (taskHistories != null) {
        response = TaskHistoryConverterUtil
            .convertTaskHistoriesToTaskHistoryResponses(taskHistories.getContent(), false);
      }
      isSuccess = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error(
          "Error retrieving product history for given task code {}. MandatoryRequestParam: {} error {}",
          taskCode, mandatoryRequestParam, e.getMessage(), e);
    }
    return new GdnRestListResponse<TaskHistoryResponse>(errorMessage, null, isSuccess, response,
        new PageMetaData(size, page, response.size()), requestId);
  }
}
