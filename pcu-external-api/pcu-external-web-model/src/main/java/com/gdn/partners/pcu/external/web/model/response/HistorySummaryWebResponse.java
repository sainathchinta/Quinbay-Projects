package com.gdn.partners.pcu.external.web.model.response;

import java.io.Serializable;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
public class HistorySummaryWebResponse implements Serializable {
  private static final long serialVersionUID = -1646986046317591187L;

  private Date accessTime;
  private String gdnSku;
  private String gdnName;
  private String changedBy;
  private String oldValues;
  private String newValues;
  private String activity;
}
