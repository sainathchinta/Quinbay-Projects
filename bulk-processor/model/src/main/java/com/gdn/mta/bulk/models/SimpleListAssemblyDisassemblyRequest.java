package com.gdn.mta.bulk.models;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleListAssemblyDisassemblyRequest extends BaseRequest {

  private static final long serialVersionUID = 666964291675361223L;

  private List<AssemblyDisassemblyRequest> value = new ArrayList<>();
}
