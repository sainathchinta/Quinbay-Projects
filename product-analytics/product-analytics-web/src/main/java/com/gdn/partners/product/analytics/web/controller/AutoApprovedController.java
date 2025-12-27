package com.gdn.partners.product.analytics.web.controller;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.product.analytics.model.AutoApprovedApiPath;
import com.gdn.partners.product.analytics.service.AutoApprovedService;
import com.gdn.partners.product.analytics.service.UserFeedbackService;
import com.gdn.partners.product.analytics.web.model.AutoApprovedListWebResponse;
import com.gdn.partners.product.analytics.web.model.ProductAssigneeChangeResponse;
import com.gdn.partners.product.analytics.web.model.UserFeedbackResponse;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedAssigneeRequest;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedSelectedDownloadRequest;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedWebRequest;
import com.gdn.partners.product.analytics.web.model.request.UserFeedbackRequest;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@Slf4j
@RestController
@Tag(name = "Auto Approved Products", description = "Auto Approved Products")
@RequestMapping(value = AutoApprovedApiPath.BASE_PATH)
public class AutoApprovedController {

  @Autowired
  private AutoApprovedService autoApprovedService;

  @Autowired
  private UserFeedbackService userFeedbackService;

  @Operation(summary =  "Get Auto approved products list")
  @PostMapping(value = AutoApprovedApiPath.AUTO_APPROVED_PRODUCTS_LIST_API,
    produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestListResponse<AutoApprovedListWebResponse> fetchAutoApprovedProductsList(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "50") int size,
    @RequestBody AutoApprovedWebRequest request) throws Exception {
    log.info("fetching auto approved products for request - {}, page - {}, size - {}", request,
      page, size);

    Page<AutoApprovedListWebResponse> autoApprovedListWebResponses =
      autoApprovedService.fetchListOfAutoApprovedProducts(request, page, size);
    return new GdnRestListResponse<>(null, null, true, autoApprovedListWebResponses.getContent(),
      new PageMetaData(size, page, autoApprovedListWebResponses.getTotalElements()), requestId);
  }

  @Operation(summary = "updating user feedback for auto approved product")
  @PostMapping(value = AutoApprovedApiPath.UPSERT_USER_FEEDBACK, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnBaseRestResponse updateUserFeedback(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("productCode") String productCode, @RequestBody UserFeedbackRequest request) throws Exception {
    log.info("updating user feedback for productCode = {} ", productCode);
    userFeedbackService.updateUserFeedbackForAutoApprovedProduct(productCode, request);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @PostMapping(value = AutoApprovedApiPath.UPDATE_ASSIGNEE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update assignee for auto approved products")
  public GdnRestListResponse<ProductAssigneeChangeResponse> updateAssignedTo(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username,
    @RequestBody AutoApprovedAssigneeRequest assigneeRequest) throws Exception {
    log.info("Updating assignee for {} ", assigneeRequest.getProductCode());
    List<ProductAssigneeChangeResponse> responses =
      autoApprovedService.updateAssignee(assigneeRequest);
    return new GdnRestListResponse<>(null, null, true, responses,
      new PageMetaData(0, 0, responses.size()), requestId);
  }

  @Operation(summary = "Get User feedback for auto-approved product")
  @GetMapping(value = AutoApprovedApiPath.GET_USER_FEEDBACK_FOR_AUTO_APPROVED_PRODUCTS, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestSingleResponse<UserFeedbackResponse> getUserFeedbackForAutoApprovedProducts(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("productCode") String productCode) throws Exception {
    log.info("Get user feedback for auto approved products = {} ", productCode);
    return new GdnRestSingleResponse<>(userFeedbackService.fetchUserFeedbackResponse(productCode), requestId);
  }

  @Operation(summary = "Get Auto approved products list for selected items")
  @PostMapping(value = AutoApprovedApiPath.AUTO_APPROVED_PRODUCTS_SELECTED_DOWNLOAD, produces =
    MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestListResponse<AutoApprovedListWebResponse> fetchAutoApprovedSelectedProductsList(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestBody AutoApprovedSelectedDownloadRequest request) throws Exception {
    log.info("fetching auto approved products for request - {}", request);
    List<AutoApprovedListWebResponse> autoApprovedListWebResponses =
      autoApprovedService.fetchSelectedItemsOfAutoApprovedProducts(request);
    return new GdnRestListResponse<>(null, null, true, autoApprovedListWebResponses,
      new PageMetaData(0, 0, autoApprovedListWebResponses.size()), requestId);
  }
}
