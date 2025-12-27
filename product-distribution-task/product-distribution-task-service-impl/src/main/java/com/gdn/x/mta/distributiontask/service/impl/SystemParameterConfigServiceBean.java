package com.gdn.x.mta.distributiontask.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.common.base.GdnPreconditions.checkState;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.cache.CacheKeys;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.mta.distributiontask.dao.api.SystemParameterConfigRepository;
import com.gdn.x.mta.distributiontask.model.SystemParameterConfig;
import com.gdn.x.mta.distributiontask.request.SystemParameterConfigRequest;
import com.gdn.x.mta.distributiontask.service.api.SystemParameterConfigHistoryService;
import com.gdn.x.mta.distributiontask.service.api.SystemParameterConfigService;
import joptsimple.internal.Strings;

@Slf4j
@Service
public class SystemParameterConfigServiceBean implements SystemParameterConfigService {

  private static final String VARIABLE_MUST_NOT_BE_BLANK = "variable must not be blank";
  private static final String STORE_ID_MUST_NOT_BE_BLANK = "storeId must not be blank";
  private static final String SYSTEM_PARAMETER_IS_NOT_FOUND = "System parameter '%s' is not found for storeId '%s'";
  private static final String VALUE_MUST_NOT_BE_BLANK = "value must not be blank";
  private static final String DESCRIPTION_MUST_NOT_BE_BLANK = "description must not be blank";

  @Autowired
  private SystemParameterConfigRepository systemParameterConfigRepository;

  @Autowired
  private SystemParameterConfigHistoryService systemParameterConfigHistoryService;

  @Value("${redis.cache.enabled}")
  private boolean redisCacheEnabled;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @Autowired
  private ApplicationContext applicationContext;

  private SystemParameterConfigServiceBean getApplicationCacheServiceBean() {
    return applicationContext.getBean(SystemParameterConfigServiceBean.class);
  }

  @Override
  @Transactional(readOnly =true, rollbackFor = Exception.class)
  public SystemParameterConfig findValueByStoreIdAndVariable(String storeId, String variable) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(variable), VARIABLE_MUST_NOT_BE_BLANK);
    SystemParameterConfig systemParameter = systemParameterConfigRepository.findByStoreIdAndVariable(storeId, variable);
    checkState(Objects.nonNull(systemParameter), String.format(SYSTEM_PARAMETER_IS_NOT_FOUND, variable, storeId));
    return systemParameter;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void delete(String storeId, String username, String variable) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(variable), VARIABLE_MUST_NOT_BE_BLANK);
    SystemParameterConfig systemParameterConfig = systemParameterConfigRepository.findByStoreIdAndVariable(storeId, variable);
    checkState(Objects.nonNull(systemParameterConfig), String.format(SYSTEM_PARAMETER_IS_NOT_FOUND, variable, storeId));
    systemParameterConfigRepository.deleteById(systemParameterConfig.getId());
    systemParameterConfigHistoryService.saveHistoryDelete(systemParameterConfig, username);
    if (redisCacheEnabled) {
      getApplicationCacheServiceBean().evictSystemParametersCache(storeId);
    } else {
      log.info("Evicting system parameters cache from caffeine using store id : {}", storeId);
      kafkaPublisher.send(kafkaTopicPropertiesConsumer.getSystemParameterCaffeineCacheEvictEvent(),
          storeId);
    }
  }

  @Override
  @CacheEvict(value = CacheKeys.SYSTEM_PARAM_SWITCHES, key = "#storeId")
  public void evictSystemParametersCache(String storeId) {
    log.info("Evicting system parameters cache from redis using store id : {}", storeId);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void insert(String storeId, String username, SystemParameterConfigRequest systemParameterConfigRequest) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(systemParameterConfigRequest.getVariable()), VARIABLE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(systemParameterConfigRequest.getValue()), VALUE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(systemParameterConfigRequest.getDescription()), DESCRIPTION_MUST_NOT_BE_BLANK);
    SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
    BeanUtils.copyProperties(systemParameterConfigRequest, systemParameterConfig);
    systemParameterConfig.setCreatedBy(username);
    systemParameterConfig.setUpdatedBy(username);
    systemParameterConfig.setCreatedDate(new Date());
    systemParameterConfig.setUpdatedDate(new Date());
    systemParameterConfig.setStoreId(storeId);
    SystemParameterConfig savedSystemParameterConfig = systemParameterConfigRepository.save(systemParameterConfig);
    systemParameterConfigHistoryService.saveHistoryUpdate(savedSystemParameterConfig, Strings.EMPTY);
    if (redisCacheEnabled) {
      getApplicationCacheServiceBean().evictSystemParametersCache(storeId);
    } else {
      log.info("Evicting system parameters cache from caffeine using store id : {}", storeId);
      kafkaPublisher.send(kafkaTopicPropertiesConsumer.getSystemParameterCaffeineCacheEvictEvent(),
          storeId);
    }
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void update(String storeId, String username, SystemParameterConfigRequest systemParameterConfigRequest) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(systemParameterConfigRequest.getVariable()), VARIABLE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(systemParameterConfigRequest.getValue()), VALUE_MUST_NOT_BE_BLANK);
    SystemParameterConfig oldSystemParameter = systemParameterConfigRepository.findByStoreIdAndVariable(
        storeId, systemParameterConfigRequest.getVariable());
    checkState(Objects.nonNull(oldSystemParameter),
        String.format(SYSTEM_PARAMETER_IS_NOT_FOUND, storeId, systemParameterConfigRequest.getVariable()));
    String oldValue = oldSystemParameter.getValue();
    oldSystemParameter.setValue(systemParameterConfigRequest.getValue());
    oldSystemParameter.setUpdatedBy(username);
    oldSystemParameter.setUpdatedDate(new Date());
    SystemParameterConfig newSystemParameter = systemParameterConfigRepository.save(oldSystemParameter);
    systemParameterConfigHistoryService.saveHistoryUpdate(newSystemParameter, oldValue);
    if (redisCacheEnabled) {
      getApplicationCacheServiceBean().evictSystemParametersCache(storeId);
    } else {
      log.info("Evicting system parameters cache from caffeine using store id : {}", storeId);
      kafkaPublisher.send(kafkaTopicPropertiesConsumer.getSystemParameterCaffeineCacheEvictEvent(),
          storeId);
    }
  }

  @Override
  @Cacheable(value = CacheKeys.SYSTEM_PARAM_SWITCHES, key = "#storeId", unless = "#result == null")
  public Map<String, String> findSwitchValues(String storeId) {
    List<SystemParameterConfig> systemParameterConfigList =
      systemParameterConfigRepository.findByStoreIdAndMarkForDeleteFalse(storeId);
    return systemParameterConfigList.stream().collect(
      Collectors.toMap(SystemParameterConfig::getVariable, SystemParameterConfig::getValue));
  }
}
