package com.gdn.mta.bulk.service;

import static com.gdn.common.helper.GdnDateHelper.now;

import java.util.Date;
import java.util.Objects;
import java.util.Set;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.entity.UnifiedBulkDownloadEvent;
import com.gdn.mta.bulk.repository.UnifiedBulkDownloadRepository;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class UnifiedBulkDownloadServiceBean implements UnifiedBulkDownloadService {

  private static final String BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK = "Business partner code must not be blank";

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private UnifiedBulkDownloadRepository unifiedBulkDownloadRepository;

  @Override
  public void updatePickUpPointFlag(String businessPartnerCode) {
    GdnPreconditions
        .checkArgument(StringUtils.isNotBlank(businessPartnerCode), BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    UnifiedBulkDownloadEvent bulkDownloadEvent =
        unifiedBulkDownloadRepository.findByStoreIdAndBusinessPartnerCode(Constant.STORE_ID, businessPartnerCode);
    if (Objects.isNull(bulkDownloadEvent)) {
      bulkDownloadEvent = getUnifiedBulkDownloadEvent(businessPartnerCode, Constant.XBP);
    }
    bulkDownloadEvent.setPickupPointUpdated(true);
    unifiedBulkDownloadRepository.save(bulkDownloadEvent);
  }

  private UnifiedBulkDownloadEvent getUnifiedBulkDownloadEvent(String businessPartnerCode, String source) {
    UnifiedBulkDownloadEvent unifiedBulkDownloadEvent = new UnifiedBulkDownloadEvent();
    unifiedBulkDownloadEvent.setBusinessPartnerCode(businessPartnerCode);
    unifiedBulkDownloadEvent.setCreatedBy(source);
    unifiedBulkDownloadEvent.setUpdatedBy(source);
    unifiedBulkDownloadEvent.setCreatedDate(now());
    unifiedBulkDownloadEvent.setUpdatedDate(now());
    return unifiedBulkDownloadEvent;
  }

  @Override
  @Async
  @Transactional(readOnly = false)
  public void abortPendingDownloadProcesses(String storeId) {
    SystemParameterConfig systemParameter = this.systemParameterConfigService
        .findValueByStoreIdAndVariable(storeId, Constant.ABORT_PROCESS_PENDING_BEFORE_SECONDS);
    int seconds = Integer.parseInt(systemParameter.getValue());
    Date updatedDate = ProcessorUtils.getEarlierDateBySeconds(seconds);
    log.info("Aborting all pending tasks for bulk download for storeId {} before {}", storeId, updatedDate);
    unifiedBulkDownloadRepository
        .updatePendingDownloadProcesses(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), updatedDate);
    log.info("Successfully completed aborting the pending download processes");
  }

  @Override
  public UnifiedBulkDownloadEvent getByStoreIdAndBusinessPartnerCode(String storeId, String businessPartnerCode) {
    return unifiedBulkDownloadRepository.findByStoreIdAndBusinessPartnerCode(storeId, businessPartnerCode);
  }

  @Override
  public void updateStatusAndLastDownloadTime(String status, String businessPartnerCode, Set<String> pickupPointCodes) {
    UnifiedBulkDownloadEvent bulkDownloadEvent =
        unifiedBulkDownloadRepository.findByStoreIdAndBusinessPartnerCode(Constant.STORE_ID, businessPartnerCode);
    if (Objects.nonNull(bulkDownloadEvent)) {
      if (StringUtils.equalsIgnoreCase(status, Constant.UNIFIED_DOWNLOAD_COMPLETE)){
        bulkDownloadEvent.setLastDownloadedTime(now());
        bulkDownloadEvent.setBrandUpdated(false);
        bulkDownloadEvent.setPickupPointUpdated(false);
      }
      bulkDownloadEvent.setDownloadStatus(status);
      bulkDownloadEvent.setUpdatedDate(now());
      bulkDownloadEvent.setUpdatedBy(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
    } else {
      bulkDownloadEvent = setUnifiedBulkDownloadEvent(businessPartnerCode, status);
    }
    if (CollectionUtils.isNotEmpty(pickupPointCodes)) {
      bulkDownloadEvent.setPickupPointUpdated(true);
    }
    unifiedBulkDownloadRepository.save(bulkDownloadEvent);
  }

  private UnifiedBulkDownloadEvent setUnifiedBulkDownloadEvent(String businessPartnerCode, String status) {
    UnifiedBulkDownloadEvent unifiedBulkDownloadEvent = new UnifiedBulkDownloadEvent();
    unifiedBulkDownloadEvent.setBusinessPartnerCode(businessPartnerCode);
    unifiedBulkDownloadEvent.setCreatedBy(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
    unifiedBulkDownloadEvent.setUpdatedBy(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
    unifiedBulkDownloadEvent.setCreatedDate(now());
    unifiedBulkDownloadEvent.setUpdatedDate(now());
    unifiedBulkDownloadEvent.setLastDownloadedTime(now());
    unifiedBulkDownloadEvent.setDownloadStatus(status);
    unifiedBulkDownloadEvent.setStoreId(MDC.get(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER));
    return unifiedBulkDownloadEvent;
  }

  @Override
  public void updateBrandFlag(String businessPartnerCode) {
    GdnPreconditions
        .checkArgument(StringUtils.isNotBlank(businessPartnerCode), BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    UnifiedBulkDownloadEvent bulkDownloadEvent =
        unifiedBulkDownloadRepository.findByStoreIdAndBusinessPartnerCode(Constant.STORE_ID, businessPartnerCode);
    if (Objects.isNull(bulkDownloadEvent)) {
      bulkDownloadEvent = getUnifiedBulkDownloadEvent(businessPartnerCode, Constant.PCB);
    }
    bulkDownloadEvent.setBrandUpdated(true);
    unifiedBulkDownloadRepository.save(bulkDownloadEvent);
  }
}