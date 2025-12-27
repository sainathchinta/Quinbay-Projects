package com.gdn.mta.bulk.service.download;

import java.util.Calendar;
import java.util.List;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.entity.BulkDownloadEntity;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.CampaignProductDownloadRequest;
import com.gdn.mta.bulk.repository.BulkDownloadAuditRepository;


/**
 * Created by keshashah on 04/11/16.
 */
@Service
public class BulkDownloadAuditServiceImpl implements BulkDownloadAuditService {
  private static final Logger LOGGER = LoggerFactory.getLogger(BulkDownloadAuditServiceImpl.class);
  private static final int MAX_WIDTH = 3500;
  private static final int MAX_WIDTH_FOR_DESCRIPTION = 3900;

  @Autowired
  private BulkDownloadAuditRepository bulkDownloadAuditRepository;

  @Autowired
  @Qualifier("objectMapper")
  private ObjectMapper mapper;

  @Transactional
  @Override
  public void createAuditLog(BulkDownloadRequest request, String status) throws Exception {
    LOGGER.info("BULK DOWNLOAD : Creating audit for request {} and status {} ", request, status);
    BulkDownloadEntity existingDownloadEntity = bulkDownloadAuditRepository.findByRequestId(request.getRequestId());
    if (Objects.isNull(existingDownloadEntity)) {
      String description;
      String primaryIdentifier = null;
      BulkDownloadEntity entity = new BulkDownloadEntity();
      entity.setRequestId(request.getRequestId());
      entity.setBusinessPartnerCode(request.getMerchantId());
      entity.setCreatedBy(request.getUsername());
      entity.setCreatedDate(Calendar.getInstance().getTime());
      entity.setEntityType(request.getBulkProcessEntity().name());
      if(request instanceof CampaignProductDownloadRequest){
        CampaignProductDownloadRequest campaignProductDownloadRequest = (CampaignProductDownloadRequest) request;
        primaryIdentifier = campaignProductDownloadRequest.getCampaignCode();
      }
      if (BulkProcessEntity.CAMPAIGN_PRODUCT.equals(request.getBulkProcessEntity())) {
        description = StringUtils.abbreviate(this.mapper.writeValueAsString(request),
          MAX_WIDTH_FOR_DESCRIPTION);
      } else {
        description = StringUtils.abbreviate(request.toString(), MAX_WIDTH);
      }
      entity.setDescription(description);
      entity.setRequestBody(this.mapper.writeValueAsString(request));
      entity.setRecordsDownload(0);
      entity.setPrimaryIdentifier(primaryIdentifier);
      entity.setFileName(request.getFilename());
      entity.setStatus(status);
      entity.setErrorMessage(StringUtils.EMPTY);
      bulkDownloadAuditRepository.saveAndFlush(entity);
    } else {
      LOGGER.error("BULK DOWNLOAD : Audit for requestId {} already exists", request.getRequestId());
      throw new ApplicationException(ErrorCategory.VALIDATION,
          " Audit already exists for requestId : " + request.getRequestId());
    }
  }

  @Transactional
  @Override
  public void updateAuditLog(String requestId, String status, int recordsDownloaded,
      String errorMessage) throws Exception{
    LOGGER.info("BULK DOWNLOAD : Updating audit for requestId {} and status {} ",
        requestId, status);
    BulkDownloadEntity entity = bulkDownloadAuditRepository.findByRequestId(requestId);
    entity.setRecordsDownload(recordsDownloaded);
    entity.setStatus(status);
    entity.setErrorMessage(errorMessage);
    entity.setEndDate(Calendar.getInstance().getTime());
    bulkDownloadAuditRepository.saveAndFlush(entity);
  }

  @Override
  public List<BulkDownloadEntity> getPendingAuditLogs(String status, int limit) throws Exception {
    return bulkDownloadAuditRepository.findByStatusOrderByCreatedDateAsc(status, PageRequest.of(0, limit));
  }

  @Override
  @Transactional(readOnly = false)
  public BulkDownloadEntity saveBulkDownloadEntity(BulkDownloadEntity bulkDownloadEntity) {
    return bulkDownloadAuditRepository.save(bulkDownloadEntity);
  }

  @Override
  @Transactional(readOnly = false)
  public List<BulkDownloadEntity> saveBulkDownloadEntityList(List<BulkDownloadEntity> bulkDownloadEntityList) {
    return bulkDownloadAuditRepository.saveAll(bulkDownloadEntityList);
  }
}
