package com.gdn.x.productcategorybase.dto.request;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleStringListRequest implements Serializable {

  private static final long serialVersionUID = -6270664664583517829L;
  private List<String> request = new ArrayList<>();

}
