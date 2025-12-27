package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessImage;
import com.gdn.mta.bulk.repository.BulkProcessImageRepository;
import com.gdn.partners.bulk.util.Constant;

public class BulkProcessImageServiceBeanTest {

  private static final String STORE_ID = "10001";

  @InjectMocks
  BulkProcessImageServiceBean bulkProcessImageServiceBean;

  @Mock
  BulkProcessImageRepository bulkProcessImageRepository;

  private static final String BULK_PROCESS_CODE = "bulkCode";
  private static final String URL = "url";
  private static final String URL_1 = "url1";

  private List<String> imageUrl;
  private BulkProcessImage bulkProcessImage;
  private BulkProcessImage bulkProcessImage1;
  List<BulkProcessImage> bulkProcessImageList = new ArrayList<>();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    imageUrl = new ArrayList<>();
    imageUrl.add(URL);
    imageUrl.add(URL_1);

    bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setCompleted(true);
    bulkProcessImage1 = new BulkProcessImage();
    bulkProcessImage1.setCompleted(false);
    bulkProcessImageList.add(bulkProcessImage);
    bulkProcessImageList.add(bulkProcessImage1);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(bulkProcessImageRepository);
  }

  @Test
  public void findByBulkProcessCodeAndImageUrlTest() {
    Mockito.when(bulkProcessImageRepository.findByBulkProcessIdAndImageURLIn(new BulkProcess().getId(), imageUrl))
        .thenReturn(bulkProcessImageList);
    List<BulkProcessImage> response =
        bulkProcessImageServiceBean.findByBulkProcessCodeAndImageUrl(new BulkProcess(), imageUrl);
    Mockito.verify(bulkProcessImageRepository).findByBulkProcessIdAndImageURLIn(new BulkProcess().getId(), imageUrl);
    Assertions.assertEquals(1, response.size());
  }

  @Test
  public void saveBulkProcessImage() {
    bulkProcessImageServiceBean.saveBulkProcessImage(bulkProcessImageList);
    Mockito.verify(bulkProcessImageRepository).saveAll(bulkProcessImageList);
  }

  @Test
  public void findByStoreIdAndBulkProcessCodeTest() {
    bulkProcessImageServiceBean.findByStoreIdAndBulkProcess(Constant.STORE_ID, new BulkProcess());
    Mockito.verify(bulkProcessImageRepository)
        .findByStoreIdAndBulkProcessIdAndMarkForDeleteFalse(Constant.STORE_ID, new BulkProcess().getId());
  }

  @Test
  public void deleteDataByUpdatedDate() {
    Mockito.doNothing().when(bulkProcessImageRepository).deleteByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    this.bulkProcessImageServiceBean.deleteImageByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(bulkProcessImageRepository).deleteByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
  }

  @Test
  public void updatePendingImageDownloadsTest() {
    Date date = new Date();
    Mockito.doNothing().when(bulkProcessImageRepository)
        .updatePendingImageDownloads(STORE_ID, Arrays.asList(BULK_PROCESS_CODE), date, Constant.SYSTEM_ERROR);
    this.bulkProcessImageServiceBean.updatePendingImageDownloads(STORE_ID, Arrays.asList(BULK_PROCESS_CODE), date);
    Mockito.verify(bulkProcessImageRepository)
        .updatePendingImageDownloads(STORE_ID, Arrays.asList(BULK_PROCESS_CODE), date, Constant.SYSTEM_ERROR);
  }
}
