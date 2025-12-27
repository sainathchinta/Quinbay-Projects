package com.gdn.partners.pcu.external.client.helper;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class Response<T> {

    private Integer code;
    private String status;
    private T data;
    private Map<String, List<String>> errors;
    private Map<String, Object> metadata;
}
