package com.gdn.x.mta.distributiontask.controller;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.x.mta.distributiontask.service.api.SlaCheckerService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@Controller
@RequestMapping(value = SlaController.BASE_PATH)
@Tag(name = "Sla Controller", description = "Sla Controller")
@Slf4j
public class SlaController {
  public static final String BASE_PATH = "/sla";
  public static final String EXECUTE_JOB = "/executeSlaCheckerJob";

  @Autowired
  private SlaCheckerService slaCheckerService;

  @RequestMapping(value = EXECUTE_JOB, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "execute sla checker job", description = "execute sla checker job")
  public @ResponseBody GdnRestSimpleResponse<String> executeSlaCheckerJob(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username) throws Exception {
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    log.info("Execute SLA Checker JOB. MandatoryRequestParam: {}", mandatoryRequestParam);
    slaCheckerService.execute();
    return new GdnRestSimpleResponse<String>("", "", true, requestId, "Sla Checker Executed");
  }
}
