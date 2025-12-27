package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.service.api.MasterDataCacheService;
import com.gdn.x.product.service.task.GetMasterDataTask;

public class MasterDataConcurrentServiceImplTest {

  private static final String CODE1 = "code1";
  private static final String CODE2 = "code2";
  private static final int CONCURRENT_SIZE = 1;
  private static final String STORE_ID = "storeId";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "requestId";

  @InjectMocks
  private MasterDataConcurrentServiceImpl masterDataConcurrentServiceImpl;

  @Mock
  private MasterDataCacheService masterDataCacheService;

  @Mock
  private ExecutorService executorService;

  @Test
  public void doConcurrentCallTest() throws InterruptedException {
    Set<String> codes = new HashSet<String>();
    codes.add(MasterDataConcurrentServiceImplTest.CODE1);
    codes.add(MasterDataConcurrentServiceImplTest.CODE2);
    List<String> codes1 = new ArrayList<String>();
    codes1.add(MasterDataConcurrentServiceImplTest.CODE1);
    List<String> codes2 = new ArrayList<String>();
    codes2.add(MasterDataConcurrentServiceImplTest.CODE2);

    List<GetMasterDataTask<MasterDataItem>> listTask =
        new ArrayList<GetMasterDataTask<MasterDataItem>>();
    listTask.add(new GetMasterDataTask<MasterDataItem>(MasterDataItem.class, codes2,
        MasterDataConcurrentServiceImplTest.REQUEST_ID,
        MasterDataConcurrentServiceImplTest.USERNAME, this.masterDataCacheService));
    listTask.add(new GetMasterDataTask<MasterDataItem>(MasterDataItem.class, codes1,
        MasterDataConcurrentServiceImplTest.REQUEST_ID,
        MasterDataConcurrentServiceImplTest.USERNAME, this.masterDataCacheService));

    Map<String, MasterDataItem> expectedFinalResult = new HashMap<String, MasterDataItem>();
    expectedFinalResult.put(MasterDataConcurrentServiceImplTest.CODE1, new MasterDataItem());
    ArrayList<Future<Map<String, MasterDataItem>>> future =
        new ArrayList<Future<Map<String, MasterDataItem>>>();
    future.add(new Future<Map<String, MasterDataItem>>() {

      @Override
      public boolean cancel(boolean mayInterruptIfRunning) {
        return false;
      }

      @Override
      public Map<String, MasterDataItem> get() throws InterruptedException, ExecutionException {
        return expectedFinalResult;
      }

      @Override
      public Map<String, MasterDataItem> get(long timeout, TimeUnit unit)
          throws InterruptedException, ExecutionException, TimeoutException {
        return null;
      }

      @Override
      public boolean isCancelled() {
        return false;
      }

      @Override
      public boolean isDone() {
        return false;
      }
    });
    when(this.executorService.invokeAll(listTask)).thenReturn(future);

    Map<String, MasterDataItem> result =
        this.masterDataConcurrentServiceImpl.doConcurrentCall(MasterDataItem.class,
            MasterDataConcurrentServiceImplTest.USERNAME,
            MasterDataConcurrentServiceImplTest.REQUEST_ID, codes,
            MasterDataConcurrentServiceImplTest.CONCURRENT_SIZE, false);

    verify(this.executorService).invokeAll(listTask);
    assertEquals(result, expectedFinalResult);

  }

  @BeforeEach
  public void setUp() {
    openMocks(this);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.executorService);
    verifyNoMoreInteractions(this.masterDataCacheService);
  }
}
