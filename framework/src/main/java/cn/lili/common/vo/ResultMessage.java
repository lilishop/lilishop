package cn.lili.common.vo;


import lombok.Data;

import java.io.Serializable;

/**
 * 前后端交互VO
 *
 * @author Chopper
 */
@Data
public class ResultMessage<T> implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 成功标志
     */
    private boolean success;

    /**
     * 消息
     */
    private String message;

    /**
     * 返回代码
     */
    private Integer code;

    /**
     * 时间戳
     */
    private long timestamp = System.currentTimeMillis();

    /**
     * 结果对象
     */
    private T result;
}
