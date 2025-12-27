package com.gdn.mta.bulk.service;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessVideo;
import com.gdn.mta.bulk.repository.BulkProcessVideoRepository;
import com.gdn.partners.bulk.util.Constant;

public class BulkProcessVideoServiceBeanTest {

  private static final String BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  private static final String URL = "URL";
  @InjectMocks
  BulkProcessVideoServiceBean bulkProcessVideoServiceBean;

  @Mock
  BulkProcessVideoRepository bulkProcessVideoRepository;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void findByStoreIdAndBulkProcessCodeTest() {
    bulkProcessVideoServiceBean.findByStoreIdAndBulkProcess(Constant.STORE_ID, new BulkProcess());
    Mockito.verify(bulkProcessVideoRepository)
        .findByStoreIdAndBulkProcessCodeAndMarkForDeleteFalse(Constant.STORE_ID, new BulkProcess().getId());
  }

  @Test
  public void findByStoreIdAndBulkProcessCodeAndUploadedURLTest() {
    bulkProcessVideoServiceBean.findByStoreIdAndBulkProcessCodeAndUploadedURL(Constant.STORE_ID, BUSINESS_PARTNER_CODE,
        URL);
    Mockito.verify(bulkProcessVideoRepository)
        .findByStoreIdAndBulkProcessCodeAndUploadedURL(Constant.STORE_ID, BUSINESS_PARTNER_CODE, URL);
  }

  @Test
  public void testSaveAllBulkProcessVideo() {
    BulkProcessVideo video1 = new BulkProcessVideo();
    BulkProcessVideo video2 = new BulkProcessVideo();
    List<BulkProcessVideo> videoList = Arrays.asList(video1, video2);
    Mockito.when(bulkProcessVideoRepository.saveAll(videoList)).thenReturn(videoList);
    List<BulkProcessVideo> result = bulkProcessVideoServiceBean.saveAllBulkProcessVideo(videoList);
    Assertions.assertEquals(2, result.size());
    Mockito.verify(bulkProcessVideoRepository).saveAll(videoList);
  }

  @Test
  public void testSaveBulkProcessVideo() {
    BulkProcessVideo video1 = new BulkProcessVideo();
    Mockito.when(bulkProcessVideoRepository.save(video1)).thenReturn(video1);
    BulkProcessVideo result = bulkProcessVideoServiceBean.saveBulkProcessVideo(video1);
    Mockito.verify(bulkProcessVideoRepository).save(video1);
    Assertions.assertNotNull(result);
  }

  @Test
  public void testSaveBulkProcessVideoNull() {
    BulkProcessVideo video2 = new BulkProcessVideo();
    Mockito.when(bulkProcessVideoRepository.save(video2)).thenReturn(null);
    BulkProcessVideo result = bulkProcessVideoServiceBean.saveBulkProcessVideo(video2);
    Mockito.verify(bulkProcessVideoRepository).save(video2);
    Assertions.assertNull(result);
  }
}
