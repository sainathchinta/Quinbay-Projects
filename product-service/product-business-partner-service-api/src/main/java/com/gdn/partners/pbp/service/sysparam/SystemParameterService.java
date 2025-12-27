package com.gdn.partners.pbp.service.sysparam;

import java.util.List;


public interface SystemParameterService {

  /**
   * reload system parameters
   * 
   * @param fullReload if set to true, then reset all the values based on sysparam.properties, else
   *        just reload from redis only (changes on sysparam.properties won't be considered)
   */
  void reload(boolean fullReload);

  /**
   * reload new system parameters only from sysparam.properties
   */
  void reset();

  void setParameter(String key, String value);

  String getParameter(String key);

  /**
   * To check whether the sysparam.properties key-value pairs have been loaded to redis or not
   * 
   * @return true if already loaded, false otherwise
   */
  boolean isInitialized();

  List<String> keys();
}
