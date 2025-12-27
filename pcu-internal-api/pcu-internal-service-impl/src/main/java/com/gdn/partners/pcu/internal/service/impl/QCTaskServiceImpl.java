package com.gdn.partners.pcu.internal.service.impl;

import static com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName.PRODUCT_QC_APPROVED_TASK_EVENT_NAME;

import com.gdn.partners.pcu.internal.client.model.response.DistributionProductResponse;
import com.gdn.partners.pcu.internal.properties.KafkaTopicProperties;
import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.internal.service.impl.event.model.RetryFinalQCEventModel;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.config.ProductApprovalDetailStatus;
import com.gdn.mta.domain.event.modal.ProductApprovalDetailStatusEvent;
import com.gdn.partners.pcu.internal.client.feign.PDTFeign;
import com.gdn.partners.pcu.internal.service.QCTaskService;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.web.model.request.RejectProductWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SummaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BusinessPartnerWebResponse;
import org.apache.commons.lang.StringUtils;
import com.gdn.partners.pcu.internal.web.model.response.DistributionProductWebResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.PDTProductDomainEventModelResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class QCTaskServiceImpl implements QCTaskService {

  @Autowired
  private PDTFeign pdtFeign;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${retry.final.qc.flag}")
  private boolean retryFinalQCFlag;

  public Page<DistributionProductWebResponse> filterQCProductList(SummaryFilterWebRequest summaryFilterWebRequest,
      int page, int size) {
    GdnRestListResponse<DistributionProductResponse> response = pdtFeign
        .filterProduct(summaryFilterWebRequest.getStatus(), page, size,
            RequestHelper.toProductListRequest(summaryFilterWebRequest));
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(ResponseHelper.toDistributionProductWebResponseList(response.getContent()),
        PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
  }

  @Override
  public void retryApproveQc(String productCode) {
    if (retryFinalQCFlag) {
      RetryFinalQCEventModel eventModel = new RetryFinalQCEventModel(productCode);
      eventModel.setTimestamp(System.currentTimeMillis());
      log.info("Publishing event {} for productCode = {} retryFinalQCEventModel = {} ",
          kafkaTopicProperties.getRetryFinalQcEventName(), productCode, eventModel);
      this.kafkaPublisher.send(kafkaTopicProperties.getRetryFinalQcEventName(), productCode,
          eventModel);
    } else {
      this.kafkaPublisher.send(DomainEventName.PRODUCT_STATUS_UPDATE_TRACKER, productCode,
          new ProductApprovalDetailStatusEvent(productCode,
              ProductApprovalDetailStatus.MTA_Approve_Product_Request_Sent_PDT,
              System.currentTimeMillis()));

      GdnRestSingleResponse<PDTProductDomainEventModelResponse> pdtProductDomainEventModel =
          pdtFeign.getPDTDomainModelResponseByCode(productCode);
      ResponseHelper.validateResponse(pdtProductDomainEventModel);

      this.kafkaPublisher.send(DomainEventName.PRODUCT_STATUS_UPDATE_TRACKER, productCode,
          new ProductApprovalDetailStatusEvent(productCode,
              ProductApprovalDetailStatus.MTA_Retry_Approve_Product_Kafka_Sent_PBP,
              System.currentTimeMillis()));
      kafkaPublisher.send(PRODUCT_QC_APPROVED_TASK_EVENT_NAME, productCode,pdtProductDomainEventModel);
    }
  }

  @Override
  public void rejectProduct(RejectProductWebRequest rejectProductWebRequest) throws Exception {
    GdnBaseRestResponse response =
        pdtFeign.rejectQCProduct(RequestHelper.toRejectProductListRequest(rejectProductWebRequest));
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void approveProduct(String productCode, String productId) throws Exception {
    this.kafkaPublisher.send(DomainEventName.PRODUCT_STATUS_UPDATE_TRACKER, productCode,
        new ProductApprovalDetailStatusEvent(productCode,
            ProductApprovalDetailStatus.MTA_Approve_Product_Request_Sent_PDT, System.currentTimeMillis()));

    GdnBaseRestResponse response = pdtFeign.approveQCProduct(productId);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public Page<BusinessPartnerWebResponse> getBusinessPartnerList(String state, String keyword, int page, int size) {
    boolean isSearch = false;
    if (StringUtils.isNotBlank(keyword)) {
      isSearch = true;
    }
    GdnRestListResponse<ProductBusinessPartnerMapperResponse> response =
        pdtFeign.filterProductBusinessPartnerMapperByWorkFlowState(page, size, isSearch, keyword, state);
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(ResponseHelper.toBusinessPartnerWebResponseList(response.getContent()),
        PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
  }
}
