package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.math.BigDecimal;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.HandlingFeeRequest;
import com.gdn.x.product.model.vo.HandlingFeeResponse;
import com.gdn.x.product.service.api.HandlingFeeService;
import com.gdn.x.product.service.api.SystemParameterService;

@Service
public class HandlingFeeServiceImpl implements HandlingFeeService {

  private static final String CATENTRY_IDS_MUST_NOT_BE_BLANK = "catentryIds must not be blank";
  private static final String SYSTEM_PARAMETER_MUST_NOT_BE_BLANK =
      "systemParameter must not be blank";
  private static final String STORE_ID_MUST_NOT_BE_BLANK = "storeId must not be blank";

  @Autowired
  private SystemParameterService systemParameterService;

  @Override
  public HandlingFeeResponse calculateHandlingFee(String storeId,
      List<HandlingFeeRequest> handlingFeeList) {
    checkArgument(StringUtils.isNotBlank(storeId),
        HandlingFeeServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(handlingFeeList != null, HandlingFeeServiceImpl.CATENTRY_IDS_MUST_NOT_BE_BLANK);

    String systemParameter =
        this.systemParameterService.findValueByStoreIdAndVariable(storeId,
            SystemParameterNames.HANDLING_FEE).getValue();

    BigDecimal handlingFee = new BigDecimal(0);
    boolean isSpecialHandlingFee = false;
    long handlingFeeTemp = 0;
    String[] handlingFeeRuleList = systemParameter.split(";");
    for (String ruleSet : handlingFeeRuleList) {
      String[] attributeList = ruleSet.split(":");
      if (StringUtils.isEmpty(attributeList[2])) {
        continue;
      }
        String[] catentryList = attributeList[2].split(",");
        for (HandlingFeeRequest orderItem : handlingFeeList) {
          for (String itemSku : catentryList) {
            if (itemSku.equals(orderItem.getItemSku())) {
              isSpecialHandlingFee = true;
              if ("1".equals(attributeList[0])) {
                long specialHandlingFee = Long.parseLong(attributeList[1]);
                handlingFeeTemp += specialHandlingFee * orderItem.getQuantity();// 30000
              } else {
                handlingFeeTemp = Long.parseLong(attributeList[1]);
              }
              break;
            }
          }
        
      }

      if (isSpecialHandlingFee) {
        break;
      }
    }

    if (isSpecialHandlingFee) {
      handlingFee = new BigDecimal(handlingFeeTemp);
    }

    return new HandlingFeeResponse(handlingFee);
  }

  @Override
  public SystemParameter getAllSettingOfHandlingFee(String storeId) {
    checkArgument(StringUtils.isNotBlank(storeId),
        HandlingFeeServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);

    return this.systemParameterService.findValueByStoreIdAndVariable(storeId,
        SystemParameterNames.HANDLING_FEE);
  }

  @Override
  public void updateAllSettingOfHandlingFee(SystemParameter systemParameter) {
    checkArgument(systemParameter != null,
        HandlingFeeServiceImpl.SYSTEM_PARAMETER_MUST_NOT_BE_BLANK);

    this.systemParameterService.update(systemParameter);
  }
}
