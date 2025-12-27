package com.gdn.x.mta.distributiontask.controller;

import static com.gdn.x.mta.distributiontask.util.DistributionProductMessageUtil.PDT_TASK_NOT_PRESENT;

import java.util.List;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.mta.distributiontask.controller.util.ProductConverterUtil;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.RejectProductRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductResponse;
import com.gdn.x.mta.distributiontask.service.api.ProductDistributionTaskService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.QCTaskService;
import com.gdn.x.mta.distributiontask.service.api.QCTaskWrapperService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

/**
 * Created by Poornima on 9/16/16.
 */

@Controller
@RequestMapping(value = QCController.BASE_PATH)
@Tag(name = "QCController", description = "QC user service API")
@Slf4j
public class QCController {
  public static final String ROOT = "/";
  public static final String BASE_PATH = "/qcTask";
  public static final String FILTER_QC_READY_PRODUCT_SUMMARY = ROOT + "filter/product/qcready-filter-summary";
  public static final String REJECT_PRODUCT = ROOT + "reject-product";
  public static final String APPROVE_PRODUCT = ROOT + "approve-product";
  public static final String MOVE_FAILED_PRODUCT_TO_QC = ROOT + "move-failed-product-to-qc";
  public static final String RETRY_QC_PRODUCTS = ROOT + "retry-qc-products";

  @Autowired
  private ProductService productService;

  @Autowired
  private ProductDistributionTaskService productDistributionTaskService;

  @Autowired
  private QCTaskService qcTaskService;

  @Autowired
  private QCTaskWrapperService qcTaskWrapperService;

  @Autowired
  private ProductConverterUtil productConverterUtil;

  @Value("${vendor.product.labels.ordered}")
  private String vendorProductLabelsOrdered;


  @RequestMapping(value = FILTER_QC_READY_PRODUCT_SUMMARY, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation
  public GdnRestListResponse<DistributionProductResponse> filterProductSummary(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String status, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestBody ProductListRequest productListRequest)
      throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId);
    try {
      Pageable pageable = PageRequest.of(page, size);
      log.debug("Api to filter products for QC controller {}", mandatoryRequestParam);
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(status), "Kindly give appropriate input for status field");
      Page<ProductAndReviewerDetailsDTO> productAndReviewerDetailsDTOPage =
          qcTaskService.getFilterProductSummaryFromSolr(storeId, status, productListRequest, pageable);
      List<DistributionProductResponse> distributionProductResponseList =
        productConverterUtil.convertToDistributionProductResponse(productAndReviewerDetailsDTOPage, vendorProductLabelsOrdered);
      return new GdnRestListResponse<>(distributionProductResponseList,
          new PageMetaData(page, size, productAndReviewerDetailsDTOPage.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error filter products for QC controller. MandatoryRequestParam: {}", mandatoryRequestParam, e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, null, null, requestId);
    }
  }

  @RequestMapping(value = REJECT_PRODUCT, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation
  @ResponseBody
  @Deprecated
  public GdnBaseRestResponse rejectProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody RejectProductRequest rejectProductRequest)
      throws Exception {
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId);
    try {
      log.debug("Api to reject product for QC controller for product id {}, mandatoryRequestParam: {}",
          rejectProductRequest.getProductId(), mandatoryRequestParam);
      Product product = productService.findByProductId(rejectProductRequest.getProductId());
      qcTaskWrapperService.qcRejectProduct(product, rejectProductRequest.isAssignedToVendor(),
          rejectProductRequest.getRejectedReason(),
          WorkflowState.valueOf(rejectProductRequest.getRejectedType()));
    } catch (Exception e) {
      log.error("Exception occurred while rejecting product by QC, ProductId: {}, RequestId: {}",
          rejectProductRequest.getProductId(), requestId, e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = APPROVE_PRODUCT, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation
  @ResponseBody
  @Deprecated
  public GdnBaseRestResponse approveProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String productId) throws Exception {
    String errorMessage = StringUtils.EMPTY;
    boolean isSuccess = false;
    MandatoryRequestParam mandatoryRequestParam = null;
    try {
      mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      GdnPreconditions
          .checkArgument(!StringUtils.isEmpty(productId), "Cannot approve product with productId null");
      log.debug("Api to approve product by QC. mandatoryRequestParam: {}, productId: {}",
          mandatoryRequestParam, productId);
      ProductDistributionTask productDistributionTask =
          productDistributionTaskService.findByProductId(productId);
      GdnPreconditions.checkArgument(productDistributionTask != null, PDT_TASK_NOT_PRESENT);
      qcTaskService.approveProductByQC(productDistributionTask);
      isSuccess = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error("error approving product by QC. mandatoryRequestParam: {}, productId: {}",
          mandatoryRequestParam, productId, e);
    }
    return new GdnBaseRestResponse(errorMessage, null, isSuccess, requestId);
  }

  @RequestMapping(value = MOVE_FAILED_PRODUCT_TO_QC, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation
  @ResponseBody
  public GdnBaseRestResponse moveFailedProductToQC(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String productCode) throws Exception {
    String errorMessage = StringUtils.EMPTY;
    boolean isSuccess = false;
    MandatoryRequestParam mandatoryRequestParam = null;
    try {
      mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      GdnPreconditions
          .checkArgument(!StringUtils.isEmpty(productCode), "Cannot process product with productCode null");
      log.info("Api to moveFailedProductToQC. mandatoryRequestParam: {}, productCode: {}",
          mandatoryRequestParam, productCode);
      ProductDistributionTask productDistributionTask =
          productDistributionTaskService.findTopByProductProductCodeOrderByUpdatedDateDesc(productCode);
      GdnPreconditions.checkArgument(productDistributionTask != null, PDT_TASK_NOT_PRESENT);
      this.qcTaskService.moveFailedProductToQC(productDistributionTask);
      isSuccess = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error("error moveFailedProductToQC. mandatoryRequestParam: {}, productCode: {}",
          mandatoryRequestParam, productCode, e);
    }
    return new GdnBaseRestResponse(errorMessage, null, isSuccess, requestId);
  }

  @RequestMapping(value = RETRY_QC_PRODUCTS, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation
  @ResponseBody
  public GdnBaseRestResponse retryQcProducts(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam(required = false, defaultValue = "0") int qcRetryCount,
      @RequestParam(required = false, defaultValue = "0") int deltaHours,
      @RequestParam(required = false, defaultValue = "0") int batchSize) throws Exception {
    log.info("Qc products retry. MandatoryRequestParam: {}",
        MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId));
    qcTaskService.retryQcProducts(qcRetryCount, deltaHours, batchSize);
    return new GdnBaseRestResponse(true);
  }
}
