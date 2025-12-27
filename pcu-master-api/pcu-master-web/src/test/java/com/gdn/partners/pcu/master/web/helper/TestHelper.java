package com.gdn.partners.pcu.master.web.helper;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.core.security.model.Mode;
import com.gdn.partners.core.security.model.Session;

/**
 * @author Pradeep Reddy
 */
public abstract class TestHelper {

  @Autowired
  protected MockMvc mockMvc;

  private ObjectMapper objectMapper = new ObjectMapper();

  protected Session getDefaultSession(){
    Session session = new Session();
    session.setMode(new Mode());
    return session;
  }

  public String toJson(Object o) {
    try {
      return objectMapper.writeValueAsString(o);
    } catch (JsonProcessingException e) {
      return StringUtils.EMPTY;
    }
  }

}
