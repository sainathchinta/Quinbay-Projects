package com.gdn.x.mta.distributiontask.service.impl;

import java.util.Date;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.x.mta.distributiontask.dao.api.SystemParameterConfigHistoryRepository;
import com.gdn.x.mta.distributiontask.model.SystemParameterConfig;
import com.gdn.x.mta.distributiontask.model.SystemParameterConfigHistory;
import com.gdn.x.mta.distributiontask.service.api.SystemParameterConfigHistoryService;

@Service
public class SystemParameterConfigHistoryServiceBean implements
    SystemParameterConfigHistoryService {

  @Autowired
  private SystemParameterConfigHistoryRepository systemParameterConfigHistoryRepository;

  @Override
  public void saveHistoryDelete(SystemParameterConfig systemParameterConfig, String deletedBy) {
    SystemParameterConfigHistory systemParameterConfigHistory = new SystemParameterConfigHistory();
    systemParameterConfigHistory.setStoreId(systemParameterConfig.getStoreId());
    systemParameterConfigHistory.setVariable(systemParameterConfig.getVariable());
    systemParameterConfigHistory.setOldValue(systemParameterConfig.getValue());
    systemParameterConfigHistory.setNewValue(null);
    systemParameterConfigHistory.setCreatedBy(deletedBy);
    systemParameterConfigHistory.setUpdatedBy(deletedBy);
    systemParameterConfigHistory.setCreatedDate(new Date());
    systemParameterConfigHistory.setUpdatedDate(new Date());
    systemParameterConfigHistoryRepository.save(systemParameterConfigHistory);
  }

  @Override
  public void saveHistoryUpdate(SystemParameterConfig newSystemParameterConfig, String oldValue) {
    SystemParameterConfigHistory systemParameterConfigHistory = new SystemParameterConfigHistory();
    systemParameterConfigHistory.setStoreId(newSystemParameterConfig.getStoreId());
    systemParameterConfigHistory.setVariable(newSystemParameterConfig.getVariable());
    systemParameterConfigHistory.setOldValue(oldValue);
    systemParameterConfigHistory.setNewValue(newSystemParameterConfig.getValue());
    systemParameterConfigHistory.setCreatedBy(newSystemParameterConfig.getUpdatedBy());
    systemParameterConfigHistory.setUpdatedBy(newSystemParameterConfig.getUpdatedBy());
    systemParameterConfigHistory.setCreatedDate(new Date());
    systemParameterConfigHistory.setUpdatedDate(new Date());
    systemParameterConfigHistoryRepository.save(systemParameterConfigHistory);
  }
}
