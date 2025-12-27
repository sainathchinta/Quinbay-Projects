package com.gdn.partners.pcu.internal.client.feign;


import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pcu.internal.client.factory.PartnersEngineFallbackFactory;
import com.gdn.partners.pcu.internal.client.model.request.UserFilter;
import com.gdn.partners.pcu.internal.client.model.response.UserResponse;

@FeignClient(name = "partnersEngineFeign", url = "${service.partnersEngine.endpoint}", fallbackFactory = PartnersEngineFallbackFactory.class)
public interface PartnersEngineFeign {

  @RequestMapping(value = "/api/user/filter", method = RequestMethod.POST)
  ListBaseResponse<UserResponse> filter(@RequestParam("page") int page, @RequestParam("size") int size,
      @RequestBody UserFilter request);
}