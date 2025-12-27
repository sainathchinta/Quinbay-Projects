package com.gdn.mta.product.service.util;

import java.lang.reflect.Field;

import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.product.analytics.web.model.AutoQCDetailResponse;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class AutoQcMapper {

  public static Object getValueByKey(AutoQCDetailResponse autoQCDetailResponse, String keyname,
      String updatedBy) {
    try {
      if (Constants.IS_EDITED_BY_INTERNAL_USER.equalsIgnoreCase(keyname)) {
        return updatedBy;
      }
      Field field = autoQCDetailResponse.getClass().getDeclaredField(keyname);
      field.setAccessible(true);
      return field.get(autoQCDetailResponse);
    } catch (NoSuchFieldException | IllegalAccessException e) {
      log.error("Error while fetching field from auto QC response ", e);
      return null;
    }
  }

}
