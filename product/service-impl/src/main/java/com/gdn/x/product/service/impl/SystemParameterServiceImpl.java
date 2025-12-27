package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.common.base.GdnPreconditions.checkState;

import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.stereotype.Service;

import com.gdn.x.product.dao.api.SystemParameterRepository;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.SystemParameterHistoryService;
import com.gdn.x.product.service.api.SystemParameterService;

@Service
public class SystemParameterServiceImpl implements SystemParameterService {

  private static final String SYSTEM_PARAMETER_IS_NOT_FOUND =
      "System parameter '%s' is not found for storeId '%s'";

  private static final String VARIABLE_MUST_NOT_BE_BLANK = "variable must not be blank";

  private static final String STORE_ID_MUST_NOT_BE_BLANK = "storeId must not be blank";

  private static final String VALUE_MUST_NOT_BE_BLANK = "value must not be blank";

  @Autowired
  private SystemParameterRepository systemParameterRepository;

  @Autowired
  private SystemParameterHistoryService systemParameterHistoryService;

  @Autowired
  private CacheEvictHelperService cacheEvictHelperService;

  @Autowired
  @Qualifier("lettuceSystemParameterCombination")
  private LettuceConnectionFactory lettuceSystemParameterConnectionFactory;

  @Override
  public void delete(String storeId, String variable) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(variable), VARIABLE_MUST_NOT_BE_BLANK);
    List<SystemParameter> deletedParams =
        systemParameterRepository.deleteByStoreIdAndVariable(storeId, variable);
    checkState(!deletedParams.isEmpty(),
        String.format(SYSTEM_PARAMETER_IS_NOT_FOUND, variable, storeId));
    systemParameterHistoryService.saveHistoryDelete(deletedParams.get(0));
    cacheEvictHelperService.flushRedisDBByJedisConnectionFactory(lettuceSystemParameterConnectionFactory);
  }

  @Cacheable(cacheManager = Constants.SYSTEM_PARAMETER_CACHE_MANAGER, value = {
      com.gdn.x.product.enums.CacheNames.SYSTEM_PARAMETER}, unless = "#result == null")
  @Override
  public List<SystemParameter> findAll(String storeId) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    return systemParameterRepository.findAllByStoreId(storeId);
  }

  @Cacheable(cacheManager = Constants.SYSTEM_PARAMETER_CACHE_MANAGER, value = {
      com.gdn.x.product.enums.CacheNames.SYSTEM_PARAMETER}, unless = "#result == null")
  @Override
  public SystemParameter findValueByStoreIdAndVariable(String storeId, String variable) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(variable), VARIABLE_MUST_NOT_BE_BLANK);
    SystemParameter systemParameter =
        this.systemParameterRepository.findByStoreIdAndVariable(storeId, variable);
    checkState(systemParameter != null,
        String.format(SYSTEM_PARAMETER_IS_NOT_FOUND, variable, storeId));
    return systemParameter;
  }

  @Override
  public void insert(SystemParameter systemParameter) {
    checkArgument(StringUtils.isNotBlank(systemParameter.getStoreId()), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(systemParameter.getVariable()), VARIABLE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(systemParameter.getValue()), VALUE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(systemParameter.getDescription()),
        "description must not be blank");
    systemParameterRepository.save(systemParameter);
    cacheEvictHelperService.flushRedisDBByJedisConnectionFactory(lettuceSystemParameterConnectionFactory);
  }

  @Override
  public void update(SystemParameter systemParameter) {
    checkArgument(StringUtils.isNotBlank(systemParameter.getStoreId()), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(systemParameter.getVariable()), VARIABLE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(systemParameter.getValue()), VALUE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(systemParameter.getDescription()),
        "description must not be blank");
    SystemParameter oldSystemParameter = systemParameterRepository.findAndUpdate(systemParameter);
    checkState(
        oldSystemParameter != null,
        String.format(SYSTEM_PARAMETER_IS_NOT_FOUND, systemParameter.getStoreId(),
            systemParameter.getVariable()));
    systemParameterHistoryService.saveHistoryUpdate(oldSystemParameter);
    cacheEvictHelperService.flushRedisDBByJedisConnectionFactory(lettuceSystemParameterConnectionFactory);
  }


}
