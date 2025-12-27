package com.gdn.partners.pcu.internal.web.controller;


import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.AssigneeApiPath;
import com.gdn.partners.pcu.internal.service.AssigneeService;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "Assignee API")
@RestController
@RequestMapping(value = AssigneeApiPath.BASE_PATH)
@Validated
public class AssigneeController {

  @Autowired
  ClientParameterHelper clientParameterHelper;

  @Autowired
  private AssigneeService assigneeService;

  @Operation(summary = "Get assignee's by time and status filters")
  @PostMapping(value = AssigneeApiPath.ASSIGNEE_FILTER, produces = MediaType.APPLICATION_JSON_VALUE,
               consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<String> getAssigneesByTimeAndStatusFilter(@RequestBody ReviewProductsFilterRequest request,
      @RequestParam boolean activated, @RequestParam boolean viewable) {
    log.info("Assignee's by time and status filters statusFilter :{}, timeFilter", request.getStatusFilter(), request.getTimeFilter());
    List<String> assigneeResponse =
        assigneeService.getAssigneesByFilterRequestAndActivatedAndViewableFlag(request, activated, viewable);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        assigneeResponse, new Metadata());
  }
}
