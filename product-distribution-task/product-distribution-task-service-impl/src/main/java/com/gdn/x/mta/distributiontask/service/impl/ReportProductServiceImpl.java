package com.gdn.x.mta.distributiontask.service.impl;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import com.gdn.x.mta.distributiontask.domain.event.model.AddCustomerProductToIPREventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.enums.ProductSourceIPR;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.dao.api.ReportProductRepository;
import com.gdn.x.mta.distributiontask.model.ReportProduct;
import com.gdn.x.mta.distributiontask.rest.model.request.ReportProductRequest;
import com.gdn.x.mta.distributiontask.service.api.ReportProductService;
import com.gdn.x.mta.distributiontask.service.impl.util.ConverterUtil;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ReportProductServiceImpl implements ReportProductService {

  @Autowired
  private ReportProductRepository reportProductRepository;

  private static final String STORE_ID_MUST_NOT_BE_NULL = "StoreId cannot be empty";
  private static final String SKU_VALUE_MUST_NOT_BE_NULL = "SKU value cannot be empty";
  private static final String MEMBER_ID_MUST_NOT_BE_NULL = "MemberId cannot be empty";
  private static final String REASON_MUST_NOT_BE_NULL = "Reason cannot be empty";
  private static final String REPORT_ALREADY_EXISTS = "Duplicate report for the Sku";

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicPropertiesConsumer kafkaTopicProperties;

  @Value("${ipr.product.report.reasons}")
  private String iprProductReportReasons;

  @Override
  public void addReportProduct(ReportProductRequest reportProductRequest) {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    GdnPreconditions.checkArgument(StringUtils.isNoneBlank(storeId), STORE_ID_MUST_NOT_BE_NULL);
    GdnPreconditions.checkArgument(StringUtils.isNoneBlank(reportProductRequest.getItemSku()), SKU_VALUE_MUST_NOT_BE_NULL);
    GdnPreconditions.checkArgument(StringUtils.isNoneBlank(reportProductRequest.getMemberId()), MEMBER_ID_MUST_NOT_BE_NULL);
    GdnPreconditions.checkArgument(StringUtils.isNoneBlank(reportProductRequest.getReason()), REASON_MUST_NOT_BE_NULL);
    if(Objects.isNull(reportProductRepository.findByStoreIdAndMemberIdAndItemSkuAndReason(storeId,
        reportProductRequest.getMemberId(), reportProductRequest.getItemSku(), reportProductRequest.getReason()))) {
      ReportProduct reportProduct = ConverterUtil.convertToReportProduct(storeId, reportProductRequest);
      reportProductRepository.save(reportProduct);
      publishAddProductToIPREvent(reportProduct, storeId);
    } else {
      log.error("Product report already exists for {}", reportProductRequest.getItemSku());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, REPORT_ALREADY_EXISTS);
    }
  }

  private void publishAddProductToIPREvent(ReportProduct reportProduct, String storeId) {
    List<String> reasonsList =
        Arrays.asList(iprProductReportReasons.split(Constants.COMMA));
    if (reasonsList.contains(reportProduct.getReason())) {
      String itemSku = reportProduct.getItemSku();
      String productSku = itemSku.substring(0, itemSku.lastIndexOf(Constants.HYPHEN));
      AddCustomerProductToIPREventModel addCustomerProductToIprEventModel =
          AddCustomerProductToIPREventModel.builder().productSku(productSku)
              .updatedBy(ProductSourceIPR.CUSTOMER_REPORT.name()).storeId(storeId)
              .source(ProductSourceIPR.CUSTOMER_REPORT.name()).build();
      log.info("Publishing event {} for product sku: {} with payload {}",
          kafkaTopicProperties.getAddCustomerProductToIprEvent(), productSku,
          addCustomerProductToIprEventModel);
      kafkaProducer.send(kafkaTopicProperties.getAddCustomerProductToIprEvent(), productSku,
          addCustomerProductToIprEventModel);
    }
  }
}
