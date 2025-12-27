package com.gdn.mta.bulk.service.download;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.BulkDownloadEntityStatus;
import com.gdn.mta.bulk.entity.BulkDownloadEntity;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.CampaignProductDownloadRequest;
import com.gdn.mta.bulk.repository.BulkDownloadAuditRepository;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Arrays;

/**
 * Created by arie.prastowo on 1/6/2017.
 */
public class BulkDownloadAuditServiceImplTest {
  private static final String requestId = "requestId";

  @InjectMocks
  private BulkDownloadAuditServiceImpl bulkDownloadAuditServiceImpl;

  @Mock
  private BulkDownloadAuditRepository bulkDownloadAuditRepository;

  @Mock
  private ObjectMapper mapper;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
  }

  @Test
  public void createAuditLog() throws Exception {
    BulkDownloadRequest request =
        new BulkDownloadRequest.BulkRequestBuilder().bulkProcessType(BulkProcessEntity.ORDER).request(requestId)
            .build();
    String status = "status";
    Mockito.when(bulkDownloadAuditRepository.findByRequestId(request.getRequestId())).thenReturn(null);
    bulkDownloadAuditServiceImpl.createAuditLog(request, status);
    Mockito.verify(bulkDownloadAuditRepository, Mockito.times(1)).saveAndFlush(Mockito.any(BulkDownloadEntity.class));
  }

  @Test
  public void createAuditLog_Campaign() throws Exception {
    CampaignProductDownloadRequest request = CampaignProductDownloadRequest.builder().campaignCode("CAMP-12345").build();
    request.setBulkProcessEntity(BulkProcessEntity.CAMPAIGN_PRODUCT);
    String status = "status";
    Mockito.when(bulkDownloadAuditRepository.findByRequestId(request.getRequestId())).thenReturn(null);
    bulkDownloadAuditServiceImpl.createAuditLog(request, status);
    Mockito.verify(bulkDownloadAuditRepository, Mockito.times(1)).saveAndFlush(Mockito.any(BulkDownloadEntity.class));
    Mockito.verify(mapper, Mockito.times(2)).writeValueAsString(request);
  }

  @Test
  public void createAuditLogWhenEntryExistsTest() throws Exception {
    BulkDownloadRequest request =
        new BulkDownloadRequest.BulkRequestBuilder().bulkProcessType(BulkProcessEntity.ORDER).request(requestId)
            .build();
    String status = "status";
    Mockito.when(bulkDownloadAuditRepository.findByRequestId(request.getRequestId()))
        .thenReturn(new BulkDownloadEntity());
    Assertions.assertThrows(Exception.class,
        () -> bulkDownloadAuditServiceImpl.createAuditLog(request, status));
  }

  @Test
  public void updateAuditLog() throws Exception {
    String requestId = "requestId";
    String status = "status";
    int recordsDownloaded = 1;
    String errorMessage = "";
    BulkDownloadEntity entity = new BulkDownloadEntity();
    Mockito.when(bulkDownloadAuditRepository.findByRequestId(requestId)).thenReturn(entity);
    bulkDownloadAuditServiceImpl.updateAuditLog(requestId, status, recordsDownloaded, errorMessage);
    Mockito.verify(bulkDownloadAuditRepository, Mockito.times(1)).findByRequestId(requestId);
    Mockito.verify(bulkDownloadAuditRepository, Mockito.times(1)).saveAndFlush(entity);
  }

  @Test
  public void saveTest() {
    bulkDownloadAuditServiceImpl.saveBulkDownloadEntity(new BulkDownloadEntity());
    Mockito.verify(bulkDownloadAuditRepository).save(Mockito.any(BulkDownloadEntity.class));
  }

  @Test
  public void getPendingAuditLogs() throws Exception {
    bulkDownloadAuditServiceImpl.getPendingAuditLogs(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), 2);
    Mockito.verify(bulkDownloadAuditRepository)
        .findByStatusOrderByCreatedDateAsc(BulkDownloadEntityStatus.STATUS_PENDING.getStatusValue(), PageRequest.of(0, 2));
  }

  @Test
  public void saveBulkDownloadEntityListTest() {
    bulkDownloadAuditServiceImpl.saveBulkDownloadEntityList(new ArrayList<>());
    Mockito.verify(bulkDownloadAuditRepository).saveAll(new ArrayList<>());
  }
}
