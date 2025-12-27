package com.gdn.x.productcategorybase.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.common.base.GdnPreconditions.checkState;

import java.util.Arrays;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.cache.annotation.Caching;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.entity.SystemParameter;
import com.gdn.x.productcategorybase.repository.SystemParameterRepository;
import com.gdn.x.productcategorybase.service.SystemParameterService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class SystemParameterServiceImpl implements SystemParameterService {

  @Autowired
  private SystemParameterRepository systemParameterRepository;

  @Override
  @Transactional(readOnly = false)
  @CacheEvict(value = CacheNames.SYSTEM_PARAMETER_SWITCHES, key = "#systemParameter.storeId")
  public void insert(SystemParameter systemParameter) {
    validateSystemParameterRequest(systemParameter);
    systemParameterRepository.save(systemParameter);
  }

  private void validateSystemParameterRequest(SystemParameter systemParameter) {
    checkArgument(StringUtils.isNotBlank(systemParameter.getStoreId()),
        ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    checkArgument(StringUtils.isNotBlank(systemParameter.getVariable()),
        ErrorMessage.VARIABLE_MUST_NOT_BE_BLANK.getMessage());
    checkArgument(StringUtils.isNotBlank(systemParameter.getValue()),
        ErrorMessage.VALUE_MUST_NOT_BE_BLANK.getMessage());
    checkArgument(StringUtils.isNotBlank(systemParameter.getDescription()),
        ErrorMessage.DESCRIPTION_MUST_NOT_BE_BLANK.getMessage());
  }

  @Override
  @Caching(evict = {@CacheEvict(value = CacheNames.SYSTEM_PARAMETER, key = "#systemParameter.variable"),
      @CacheEvict(value = CacheNames.SYSTEM_PARAMETER_SWITCHES, key = "#systemParameter.storeId")})
  @Transactional(readOnly = false)
  public void update(SystemParameter systemParameter) {
    validateSystemParameterRequest(systemParameter);
    SystemParameter response =
        systemParameterRepository.findByStoreIdAndVariable(systemParameter.getStoreId(), systemParameter.getVariable());
    checkState(Objects.nonNull(response), ErrorMessage.VALUE_NOT_FOUND.getMessage());
    response.setValue(systemParameter.getValue());
    response.setDescription(systemParameter.getDescription());
    systemParameterRepository.save(response);
  }

  @Override
  @Caching(evict = {@CacheEvict(value = CacheNames.SYSTEM_PARAMETER, key = "#variable"),
      @CacheEvict(value = CacheNames.SYSTEM_PARAMETER_SWITCHES, key = "#storeId")})
  @Transactional(readOnly = false)
  public void delete(String storeId, String variable) {
    SystemParameter response = findByStoreIdAndVariable(storeId, variable);
    checkState(Objects.nonNull(response), ErrorMessage.VALUE_NOT_FOUND.getMessage());
    systemParameterRepository.deleteById(response.getId());
  }

  @Override
  @Cacheable(value = CacheNames.SYSTEM_PARAMETER, key = "#variable", unless = "#result == null")
  public SystemParameter findByStoreIdAndVariable(String storeId, String variable) {
    SystemParameter response = this.systemParameterRepository.findByStoreIdAndVariable(storeId, variable);
    checkState(Objects.nonNull(response),
        String.format(ErrorMessage.SYSTEM_PARAMETER_IS_NOT_FOUND.getMessage(), variable, storeId));
    return response;
  }
}
