package cn.lili.common.utils;


import cn.hutool.core.util.StrUtil;
import cn.hutool.crypto.SecureUtil;
import cn.hutool.http.HttpUtil;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletRequest;


/**
 * ip工具
 *
 * @author Chopper
 */
@Slf4j
@Component
public class IpHelper {

    /**
     * qq lbs 地区查询key
     */
    @Value("${lili.lbs.key}")
    private String key;
    /**
     * qq lbs 地区查询key
     */
    @Value("${lili.lbs.sk}")
    private String sk;

    private static final String API = "https://apis.map.qq.com";


    /**
     * 获取IP返回地理信息
     *
     * @param request 请求参数
     * @return 城市信息
     */
    public String getIpCity(HttpServletRequest request) {

        String url = "/ws/location/v1/ip?key=" + key + "&ip=" + IpUtils.getIpAddress(request);
        String sign = SecureUtil.md5(url + sk);
        url = API + url + "&sign=" + sign;
        String result = "未知";
        try {
            String json = HttpUtil.get(url, 3000);
            JsonObject jsonObject = JsonParser.parseString(json).getAsJsonObject();
            String status = jsonObject.get("status").getAsString();
            if ("0".equals(status)) {
                JsonObject address = jsonObject.get("result").getAsJsonObject().get("ad_info").getAsJsonObject();
                String nation = address.get("nation").getAsString();
                String province = address.get("province").getAsString();
                String city = address.get("city").getAsString();
                String district = address.get("district").getAsString();
                if (StrUtil.isNotBlank(nation) && StrUtil.isBlank(province)) {
                    result = nation;
                } else {
                    result = province;
                    if (StrUtil.isNotBlank(city)) {
                        result += " " + city;
                    }
                    if (StrUtil.isNotBlank(district)) {
                        result += " " + district;
                    }
                }
            }
        } catch (Exception e) {
            log.info("获取IP地理信息失败");
        }
        return result;
    }
}
