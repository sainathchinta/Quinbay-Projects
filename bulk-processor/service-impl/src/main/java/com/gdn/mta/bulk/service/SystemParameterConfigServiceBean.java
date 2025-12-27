package com.gdn.mta.bulk.service;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.common.base.GdnPreconditions.checkState;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.dto.SystemParameterConfigRequest;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.repository.SystemParameterConfigRepository;

@Service
public class SystemParameterConfigServiceBean implements SystemParameterConfigService {

  private static final String VARIABLE_MUST_NOT_BE_BLANK = "variable must not be blank";
  private static final String STORE_ID_MUST_NOT_BE_BLANK = "storeId must not be blank";
  private static final String SYSTEM_PARAMETER_IS_NOT_FOUND = "System parameter '%s' is not found for storeId '%s'";
  private static final String SYSTEM_PARAMETER_LIST_IS_NOT_FOUND = "All System parameters were "
    + "not found for variable list %s and for storeId '%s'";
  private static final String VALUE_MUST_NOT_BE_BLANK = "value must not be blank";
  private static final String DESCRIPTION_MUST_NOT_BE_BLANK = "description must not be blank";

  @Autowired
  private SystemParameterConfigRepository systemParameterConfigRepository;

  @Autowired
  private SystemParameterConfigHistoryService systemParameterConfigHistoryService;

  @Override
  public SystemParameterConfig findValueByStoreIdAndVariable(String storeId, String variable) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(variable), VARIABLE_MUST_NOT_BE_BLANK);
    SystemParameterConfig systemParameter = systemParameterConfigRepository.findByStoreIdAndVariable(storeId, variable);
    checkState(Objects.nonNull(systemParameter), String.format(SYSTEM_PARAMETER_IS_NOT_FOUND, variable, storeId));
    return systemParameter;
  }

  @Override
  public void delete(String storeId, String username, String variable) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(variable), VARIABLE_MUST_NOT_BE_BLANK);
    SystemParameterConfig systemParameterConfig = systemParameterConfigRepository.deleteByStoreIdAndVariable(storeId, variable);
    checkState(Objects.nonNull(systemParameterConfig), String.format(SYSTEM_PARAMETER_IS_NOT_FOUND, variable, storeId));
    systemParameterConfigHistoryService.saveHistoryDelete(systemParameterConfig, username);
  }

  @Override
  public void insert(String storeId, String username, SystemParameterConfigRequest systemParameterConfigRequest)
      throws Exception {
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
    SystemParameterConfig systemParameter =
        systemParameterConfigRepository.findByStoreIdAndVariable(storeId, systemParameterConfig.getVariable());
    if (Objects.isNull(systemParameter)) {
      SystemParameterConfig savedSystemParameterConfig = systemParameterConfigRepository.save(systemParameterConfig);
      systemParameterConfigHistoryService.saveHistoryUpdate(savedSystemParameterConfig, StringUtils.EMPTY);
    } else {
      throw new ApplicationException(ErrorCategory.VALIDATION,
          "Variable also exists in DB : " + systemParameter.getVariable());
    }
  }

  @Override
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
  }

  @Override
  public Map<String, SystemParameterConfig> findValueByStoreIdAndVariables(String storeId,
    List<String> variables) {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(variables), VARIABLE_MUST_NOT_BE_BLANK);
    Map<String, SystemParameterConfig> variableToSystemParameterConfigMap = Optional.ofNullable(
        this.systemParameterConfigRepository.findByStoreIdAndVariableIn(storeId, variables))
      .orElse(new ArrayList<>()).stream()
      .collect(Collectors.toMap(SystemParameterConfig::getVariable, Function.identity()));
    checkArgument(variables.size() == variableToSystemParameterConfigMap.keySet().size(),
      String.format(SYSTEM_PARAMETER_LIST_IS_NOT_FOUND, variables, storeId));
    return variableToSystemParameterConfigMap;
  }
}
