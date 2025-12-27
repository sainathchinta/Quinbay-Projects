package com.gdn.mta.bulk.service;

import static com.gdn.common.helper.GdnDateHelper.now;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Collections;
import java.util.Date;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.slf4j.MDC;

import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.entity.UnifiedBulkDownloadEvent;
import com.gdn.mta.bulk.repository.UnifiedBulkDownloadRepository;
import com.gdn.partners.bulk.util.Constant;

/**
 * Created by parvej on 30/09/2020.
 */
public class UnifiedBulkDownloadServiceBeanTest {

  private static final String DEFAULT_USERNAME = "SYSTEM";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BP_CODE";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String UPDATED_BY = "XBP";
  private static final String PICKUP_POINT_CODE = "PP_CODE";

  private UnifiedBulkDownloadEvent unifiedBulkDownloadEvent;
  private SystemParameterConfig systemParameterConfig;

  @InjectMocks
  private UnifiedBulkDownloadServiceBean unifiedBulkDownloadServiceBean;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private UnifiedBulkDownloadRepository unifiedBulkDownloadRepository;

  @Captor
  private ArgumentCaptor<UnifiedBulkDownloadEvent> unifiedBulkDownloadEventArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    systemParameterConfig =
        new SystemParameterConfig(Constant.ABORT_PROCESS_PENDING_BEFORE_SECONDS, "7200", "DESCRIPTION");
    systemParameterConfig.setStoreId(Constant.STORE_ID);
    unifiedBulkDownloadEvent = new UnifiedBulkDownloadEvent();
    unifiedBulkDownloadEvent.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @Test
  public void updatePickUpPointFlagTest() {
    Mockito.when(this.unifiedBulkDownloadRepository
        .findByStoreIdAndBusinessPartnerCode(Constant.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    this.unifiedBulkDownloadServiceBean.updatePickUpPointFlag(BUSINESS_PARTNER_CODE);
    Mockito.verify(this.unifiedBulkDownloadRepository)
        .findByStoreIdAndBusinessPartnerCode(Constant.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(this.unifiedBulkDownloadRepository).save(unifiedBulkDownloadEvent);
  }

  @Test
  public void updatePickUpPointFlagWithNewRecordTest() {
    unifiedBulkDownloadEvent.setPickupPointUpdated(true);
    unifiedBulkDownloadEvent.setCreatedBy(UPDATED_BY);
    unifiedBulkDownloadEvent.setUpdatedBy(UPDATED_BY);
    unifiedBulkDownloadEvent.setCreatedDate(now());
    unifiedBulkDownloadEvent.setUpdatedDate(now());
    Mockito.when(this.unifiedBulkDownloadRepository
        .findByStoreIdAndBusinessPartnerCode(Constant.STORE_ID, BUSINESS_PARTNER_CODE)).thenReturn(null);
    this.unifiedBulkDownloadServiceBean.updatePickUpPointFlag(BUSINESS_PARTNER_CODE);
    Mockito.verify(this.unifiedBulkDownloadRepository)
        .findByStoreIdAndBusinessPartnerCode(Constant.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(this.unifiedBulkDownloadRepository).save(Mockito.any());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.systemParameterConfigService, this.unifiedBulkDownloadRepository);
  }

  @Test
  public void abortPendingDownloadProcessesTest() {
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.ABORT_PROCESS_PENDING_BEFORE_SECONDS))
        .thenReturn(systemParameterConfig);
    Mockito.doNothing().when(unifiedBulkDownloadRepository)
        .updatePendingDownloadProcesses(Mockito.eq(DEFAULT_USERNAME), any(Date.class));
    unifiedBulkDownloadServiceBean.abortPendingDownloadProcesses(Constant.STORE_ID);
    Mockito.verify(unifiedBulkDownloadRepository)
        .updatePendingDownloadProcesses(Mockito.eq(DEFAULT_USERNAME), any(Date.class));
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.ABORT_PROCESS_PENDING_BEFORE_SECONDS);
  }

  @Test
  public void getByStoreIdAndBusinessPartnerCodeTest() {
    Mockito.when(unifiedBulkDownloadRepository
        .findByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    unifiedBulkDownloadServiceBean.getByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Mockito.verify(unifiedBulkDownloadRepository)
        .findByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void updateStatusAndLastDownloadTimeTest() {
    Mockito.when(unifiedBulkDownloadRepository
        .findByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    unifiedBulkDownloadServiceBean
        .updateStatusAndLastDownloadTime(Constant.UNIFIED_DOWNLOAD_IN_PROGRESS, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito.verify(this.unifiedBulkDownloadRepository).save(unifiedBulkDownloadEvent);
    Mockito.verify(unifiedBulkDownloadRepository)
        .findByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void updateStatusAndLastDownloadTime_withPickupPointsTest() {
    Mockito.when(unifiedBulkDownloadRepository
            .findByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    unifiedBulkDownloadServiceBean
        .updateStatusAndLastDownloadTime(Constant.UNIFIED_DOWNLOAD_IN_PROGRESS, DEFAULT_BUSINESS_PARTNER_CODE, Collections.singleton(PICKUP_POINT_CODE));
    Mockito.verify(this.unifiedBulkDownloadRepository).save(unifiedBulkDownloadEvent);
    Mockito.verify(unifiedBulkDownloadRepository)
        .findByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void updateStatusAndLastDownloadTimeToCompleteTest() {
    Mockito.when(unifiedBulkDownloadRepository
        .findByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    unifiedBulkDownloadServiceBean
        .updateStatusAndLastDownloadTime(Constant.UNIFIED_DOWNLOAD_COMPLETE, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito.verify(this.unifiedBulkDownloadRepository).save(unifiedBulkDownloadEvent);
    Mockito.verify(unifiedBulkDownloadRepository)
        .findByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void updateStatusAndLastDownloadTimeNewRecordTest() {
    unifiedBulkDownloadEvent.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    unifiedBulkDownloadEvent.setLastDownloadedTime(new Date());
    unifiedBulkDownloadEvent.setBrandUpdated(false);
    unifiedBulkDownloadEvent.setPickupPointUpdated(false);
    unifiedBulkDownloadEvent.setDownloadStatus(Constant.UNIFIED_DOWNLOAD_IN_PROGRESS);
    Mockito.when(unifiedBulkDownloadRepository
        .findByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(null);
    unifiedBulkDownloadServiceBean
        .updateStatusAndLastDownloadTime(Constant.UNIFIED_DOWNLOAD_IN_PROGRESS, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Mockito.verify(this.unifiedBulkDownloadRepository).save(unifiedBulkDownloadEventArgumentCaptor.capture());
    Assertions.assertEquals(DEFAULT_BUSINESS_PARTNER_CODE,
        unifiedBulkDownloadEventArgumentCaptor.getValue().getBusinessPartnerCode());
    Assertions.assertEquals(Constant.UNIFIED_DOWNLOAD_IN_PROGRESS,
        unifiedBulkDownloadEventArgumentCaptor.getValue().getDownloadStatus());
    Assertions.assertFalse(unifiedBulkDownloadEventArgumentCaptor.getValue().isBrandUpdated());
    Assertions.assertFalse(unifiedBulkDownloadEventArgumentCaptor.getValue().isPickupPointUpdated());
    Mockito.verify(unifiedBulkDownloadRepository)
        .findByStoreIdAndBusinessPartnerCode(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void updateBrandFlagTest() {
    Mockito.when(this.unifiedBulkDownloadRepository
        .findByStoreIdAndBusinessPartnerCode(Constant.STORE_ID, BUSINESS_PARTNER_CODE))
        .thenReturn(unifiedBulkDownloadEvent);
    this.unifiedBulkDownloadServiceBean.updateBrandFlag(BUSINESS_PARTNER_CODE);
    Mockito.verify(this.unifiedBulkDownloadRepository)
        .findByStoreIdAndBusinessPartnerCode(Constant.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(this.unifiedBulkDownloadRepository).save(unifiedBulkDownloadEventArgumentCaptor.capture());
    Assertions.assertTrue(unifiedBulkDownloadEventArgumentCaptor.getValue().isBrandUpdated());
  }

  @Test
  public void updateBrandFlagWithNewEntityTest() {
    unifiedBulkDownloadEvent.setBrandUpdated(true);
    unifiedBulkDownloadEvent.setCreatedBy(UPDATED_BY);
    unifiedBulkDownloadEvent.setUpdatedBy(UPDATED_BY);
    unifiedBulkDownloadEvent.setCreatedDate(now());
    unifiedBulkDownloadEvent.setUpdatedDate(now());
    Mockito.when(this.unifiedBulkDownloadRepository
        .findByStoreIdAndBusinessPartnerCode(Constant.STORE_ID, BUSINESS_PARTNER_CODE)).thenReturn(null);
    this.unifiedBulkDownloadServiceBean.updateBrandFlag(BUSINESS_PARTNER_CODE);
    Mockito.verify(this.unifiedBulkDownloadRepository)
        .findByStoreIdAndBusinessPartnerCode(Constant.STORE_ID, BUSINESS_PARTNER_CODE);
    Mockito.verify(this.unifiedBulkDownloadRepository).save(unifiedBulkDownloadEventArgumentCaptor.capture());
    Assertions.assertTrue(unifiedBulkDownloadEventArgumentCaptor.getValue().isBrandUpdated());
  }
}