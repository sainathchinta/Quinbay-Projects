package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.x.product.service.api.MasterDataCacheService;
import com.gdn.x.product.service.api.MasterDataConcurrentService;
import com.gdn.x.product.service.task.GetMasterDataTask;
import com.google.common.collect.Lists;

@Service
public class MasterDataConcurrentServiceImpl implements MasterDataConcurrentService {

  private static final Logger LOGGER = LoggerFactory
      .getLogger(MasterDataConcurrentServiceImpl.class);

  @Autowired
  private MasterDataCacheService masterDataCacheService;

  @Autowired
  private ExecutorService executorService;

  @Override
  public <T> Map<String, T> doConcurrentCall(Class<T> masterData, String username,
      String requestId, Set<String> codes, int concurrentSize, boolean inAllProducts) {
    List<List<String>> listOfCodes = Lists.partition(new ArrayList<String>(codes), concurrentSize);
    List<GetMasterDataTask<T>> listTasks = new ArrayList<GetMasterDataTask<T>>();
    for (List<String> splitedCodes : listOfCodes) {
      listTasks.add(new GetMasterDataTask<T>(masterData, splitedCodes, requestId, username,
          this.masterDataCacheService, inAllProducts));
    }
    Map<String, T> finalResult = new HashMap<String, T>();
    try {
      List<Future<Map<String, T>>> results = this.executorService.invokeAll(listTasks);
      for (Future<Map<String, T>> result : results) {
        finalResult.putAll(result.get());
      }
    } catch (Exception e) {
      MasterDataConcurrentServiceImpl.LOGGER.error(
          "#getMasterDataFailure executing {} , with error :{}", codes, e.getMessage(), e);
    }
    return finalResult;
  }

}
