package cn.lili.modules.wechat.util;

import cn.hutool.json.JSONUtil;
import lombok.Data;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * 微信公众号消息 数据模型
 *
 * @author Chopper
 * @version v4.0
 * @since 2020/12/11 09:52
 */
@Data
public class WechatMessageData {

    /**
     * 抬头文字
     */
    private String first;

    /**
     * 备注文字
     */
    private String remark;

    /**
     * 消息内容
     */
    private List<String> messageData;
    /**
     * 小程序消息内容
     */
    private Map<String, String> mpMessageData;

    /**
     * kids
     */
    private List<String> kids;


    /**
     * 创建data数据
     *
     * @return
     */
    public String createData() {

        Map<String, Map<String, String>> dataMap = new LinkedHashMap<>();

        //拼接开头
        dataMap.put("first", this.createValue(first));

        //拼接关键字
        for (int i = 0; i < messageData.size(); i++) {
            dataMap.put("keyword" + (i + 1), createValue(this.messageData.get(i)));
        }
        //拼接备注
        dataMap.put("remark", createValue(this.remark));

        return JSONUtil.toJsonStr(dataMap);
    }


    /**
     * 创建data数据
     *
     * @return
     */
    public Map<String, Map<String, String>> createMPData() {

        LinkedHashMap<String, Map<String, String>> dataMap = new LinkedHashMap<>();

        for (String key : mpMessageData.keySet()) {
            dataMap.put(key, createValue(mpMessageData.get(key)));
        }
        return dataMap;
    }

    /**
     * 创建统一格式的map
     *
     * @param msg
     * @return
     */
    private Map<String, String> createValue(String msg) {
        Map<String, String> map = new HashMap<>(2);
        map.put("value", msg);
        return map;
    }


}
