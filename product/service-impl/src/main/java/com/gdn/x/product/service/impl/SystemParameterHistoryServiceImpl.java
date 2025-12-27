package com.gdn.x.product.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.x.product.dao.api.SystemParameterHistoryRepository;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.entity.SystemParameterHistory;
import com.gdn.x.product.service.api.SystemParameterHistoryService;

@Service
public class SystemParameterHistoryServiceImpl implements SystemParameterHistoryService {

  @Autowired
  private SystemParameterHistoryRepository historyRepository;

  @Override
  public void saveHistoryDelete(SystemParameter oldSystemParameter) {
    SystemParameterHistory history = new SystemParameterHistory(oldSystemParameter);
    historyRepository.save(history);
  }

  @Override
  public void saveHistoryUpdate(SystemParameter oldSystemParameter) {
    SystemParameterHistory history = new SystemParameterHistory(oldSystemParameter);
    historyRepository.save(history);
  }

}
